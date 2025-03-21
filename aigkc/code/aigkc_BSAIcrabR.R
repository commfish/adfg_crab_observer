# notes ----
## fishery data request - aigkc
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 3/6/2025

# load ----
devtools::install_github("commfish/BSAIcrabR", force = T)
library(BSAIcrabR)

# data inputs ----

## dockside data
dock <- load_dockside("aigkc/data/AIGKC-1985-2024_retained_size_freq.csv", stock = "AIGKC")

## observer crab detail data
meas_pot_wiki <- load_crab_dump("aigkc/data/AIGKC-1990-2024_crab_dump.csv", stock = "AIGKC")

## count pot data
count_pot_wiki <- load_pot_dump("aigkc/data/AIGKC-1990-2024_potsum.csv", stock = "AIGKC")

## fish ticket dump
read.csv("aigkc/data/fish_ticket_dump/AIGKC FT Dump 1981 - Present.csv", na.strings = "") %>%
  mutate(Mgmt.Program.Code = as.character(Mgmt.Program.Code), 
         Date.Fishing.Began = ymd_hms(Date.Fishing.Began),
         Date.of.Landing = ymd_hms(Date.of.Landing)) %>% 
  janitor::clean_names() %>%
 # rename_at(1, ~"adfg_number") %>%
  as_tibble() -> ft_dump

## season start dates
readRDS("./aigkc/data/season_dates.RDS") -> season_dates

  

# data management ----

# count pot data
add_permit_holder(ft_dump = ft_dump, pot_data = count_pot_wiki) -> count_pot

# measure pot data

meas_pot_wiki %>% 
  # get subdistrict and permit holder from count_pot data
  dplyr::select(-subdistrict) %>%
  left_join(count_pot %>% distinct(fishery, adfg, trip, spn, sample_date, subdistrict, permit_holder)) %>%
  # remove pots without district info
  filter(!is.na(subdistrict)) -> meas_pot

# fish tickets
ft_dump %>%
  mutate(season = case_when(!is.na(season) ~ season,
                            (is.na(season) & dol_year < 2000 & month_landed < 9) ~ paste0(dol_year-1, "/", substring(dol_year, 3, 4)),
                            (is.na(season) & dol_year < 2000 & month_landed >= 9) ~ paste0(dol_year, "/", substring(dol_year+1, 3, 4)),
                            (is.na(season) & dol_year >= 2000 & month_landed < 7) ~ paste0(dol_year-1, "/", substring(dol_year, 3, 4)),
                            (is.na(season) & dol_year >= 2000 & month_landed >= 7) ~ paste0(dol_year, "/", substring(dol_year+1, 3, 4)))) %>%
  # add crab year
  mutate(crab_year = as.numeric(substring(season, 1, 4))) %>%
  # adjust crab year
  mutate(date_of_landing = as_date(date_of_landing),
         crab_year = case_when(date_of_landing > mdy(paste0("6/30/", crab_year + 1)) ~ crab_year + 1,
                               date_of_landing <= mdy(paste0("6/30/", crab_year + 1)) ~ crab_year,
                               is.na(date_of_landing) ~ crab_year)) -> ft

# retained catch from 1985-1988 from AMR reports
tibble(crab_year = c(1985:1988, 1985:1988),
       fishery = c(rep("EAG", 4), rep("WAG", 4)),
       n =  c(1400484, 1307032, 1029424, 1169427, 1410711, 2033595, 1145152, 1319006),
       lb = c(6514777, 5922425, 4431745, 5148776, 6219435, 8816319, 4825260, 5478266),
       t = lb * 0.000453592,
       effort = c(117718, 155240, 146501, 155518, 118563, 277780, 160229, 166409)) -> amr

# count pot data for cpue standardization ----
write_csv(count_pot, "aigkc/output/2025/linked_potsum_dump.csv")

# measure pot dump linked ----
write_csv(meas_pot, "aigkc/output/2025/linked_meas_dump.csv")

# fish ticket dump linked ----
write_csv(ft, "aigkc/output/2025/linked_fish_ticket_dump.csv")

# cleaned dockside data ----
write_csv(dock, "aigkc/output/2025/cleaned_dockside.csv")

# retained catch ----

get_retained_catch(ft %>% filter(crab_year >= 1989), stock = "AIGKC") %>%
  bind_rows(amr %>%
              transmute(crab_year, fishery, tot_retained_n = n, tot_retained_wt = t), .) %>%
  arrange(crab_year) %>%
  # save output
  write_csv("aigkc/output/2025/retained_catch.csv")

# total catch ----

# get total fishery effort timeseries
ft %>% 
  # mutate(fishery = case_when(fishery == "EAG" ~ paste0("OB", substring(crab_year, 3, 4)),
  #                            fishery == "WAG" ~ paste0("RB", substring(crab_year, 3, 4)))) %>%
  group_by(fishery, crab_year) %>%
  summarise(effort = sum(effort_sum, na.rm = T)) %>%
  ungroup -> ft_effort

get_total_catch(count_pot %>% mutate(fishery = subdistrict), 
                meas_pot %>% mutate(fishery = subdistrict), 
                ft_data = ft_effort, stock = "AIGKC", units = "t")  %>%
  # save output
  write_csv("aigkc/output/2025/total_catch.csv")

# number of non-zero observer pots ----

# number of observed pots with non-zero male catches
count_pot %>%
  # filter out pots that used odd gear
  filter(!(gearcode %in% c(1:3, 21:23, 80)),
         (sublegal + tot_legal) > 0) %>%
  count(crab_year,fishery, subdistrict, name = "m_nz") %>% 
  # save output
  write_csv("aigkc/output/2025/nonzero_obs_pots.csv")


# number of directed fishery vessel days ----

ft %>%
  distinct(date_fishing_began, date_of_landing, adfg_number, crab_year, fishery) %>%
  mutate(date_fishing_began = as_date(date_fishing_began),
         n_days = as.numeric((date_of_landing - date_fishing_began) + 1)) %>%
  group_by(crab_year, fishery) %>%
  summarise(n_days = sum(n_days)) %>% ungroup %>% 
  # save output
  write_csv("aigkc/output/2025/vessel_days.csv")

# number of observed vessel days ----

ft %>% 
  # join to observer data to id vessel numbers not observed in a year
  left_join(count_pot %>%
              distinct(crab_year, adfg) %>%
              rename(adfg_number = adfg) %>%
              mutate(observed_check = 1)) %>%
  mutate(observed_check = ifelse(is.na(observed_check), F, T)) %>%
  # filter for observed vessels
  filter((crab_year <= 2000 & observed_check == T) |
         (crab_year > 2000 & observed == "Y")) %>%
  # filter for trips that did not include test fisheries
  group_by(date_of_landing) %>%
  filter(!("TEST" %in% mgmt_program_code)) %>%
  ungroup %>% 
  distinct(date_fishing_began, date_of_landing, adfg_number, crab_year, fishery) %>%
  mutate(date_fishing_began = as_date(date_fishing_began),
         n_days = floor(as.numeric((date_of_landing - date_fishing_began) + 1))) %>%
  group_by(crab_year, fishery) %>%
  summarise(n_days = sum(n_days)) %>%  ungroup %>%
  # save output
  write_csv("aigkc/output/2025/observed_vessel_days.csv")

# retained catch size composition ----

get_dockside_comp(dock, by = "subdistrict") %>%
  transmute(crab_year, fishery = subdistrict, size, total) %>%
  # save output
  write_csv("aigkc/output/2025/retained_size_comp.csv")

# observer size composition ----

meas_pot %>%
  # filter out pots that used odd gear
  filter(!(gearcode %in% c(1:3, 21:23, 80))) %>% 
  get_observer_comp(by = "subdistrict") %>%
  transmute(crab_year, fishery = subdistrict, size, total) %>%
  # save output
  write_csv("aigkc/output/2025/directed_observer_size_comp.csv")

# season mid date ----

# update season dates
season_dates %>%
  bind_rows(ft %>%
              filter(crab_year > 2023) %>%
              group_by(crab_year, fishery) %>%
              summarise(start_date = as_date(min(date_fishing_began)),
                        end_date = as_date(max(date_of_landing))) %>% ungroup) %>%
  # save output
  write_csv("aigkc/output/2025/season_dates.csv")

# number of sampled dockside deliveries ----

dock %>%
  mutate(sample_id = paste(adfg, sample_date, sep = "_")) %>%
  group_by(crab_year, fishery, subdistrict) %>%
  summarise(n_deliveries = length(unique(sample_id))) %>% ungroup() %>% 
  transmute(crab_year, fishery = subdistrict, n_deliveries) %>%
  # save output
  write_csv("aigkc/output/2025/deliveries_sampled.csv")


