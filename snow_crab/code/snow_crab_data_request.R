# notes ----
## fishery data request - snow crab
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 9/16/2021


# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

# data inputs ----

## dockside data
dock <- read_csv("./snow_crab/data/SNOWCRAB-1990-2020_retained_size_freq.csv")

## observer crab detail data
obs_meas <- read_csv("./snow_crab/data/SNOWCRAB-1990-2020_crab_dump.csv")

## count pot data
pot_sum <- read_csv("./snow_crab/data/SNOWCRAB-1990-2020_potsum.csv")

## stat area fish ticket summary for all fisheries directed catch
#fish_tick <- read_csv("./snow_crab/data/bsai_crab_fish_ticket_summary_stat_area.csv")
bd_ft_summary <- read_csv("./snow_crab/data/retained_catch_all_fisheries.csv") 
ft_summary_stat_area <- read_csv("./snow_crab/data/fish_ticket_summary_stat_area.csv") 

# data management ----

## adjust historic fishery codes for data dumps
## overwrite object names
## add species code to pot_sum (all will be snow crab)
list(dock = dock, obs_meas = obs_meas, pot_sum = mutate(pot_sum, spcode = 932)) %>%
  purrr::map2(., c("dockside", "obs", "obs"), f_fish_code_adjust) %>%
  list2env(x = ., envir = .GlobalEnv)

## directed effort in all fisheries
bd_ft_summary %>%
  rename_all(tolower) %>%
  filter(dir_inc == "directed") %>%
  mutate(fishery = case_when(fishery == "snow" ~ paste0("QO", substring(year, 3, 4)),
                             fishery == "Tanner W" ~ paste0("QT", substring(year, 3, 4)),
                             fishery == "Tanner E" ~ paste0("TT", substring(year, 3, 4)),
                             fishery == "BBRKC" ~ paste0("TR", substring(year, 3, 4)))) %>%
  group_by(fishery) %>%
  summarise(effort = sum(fish_effort, na.rm = T)) %>%
  
  bind_rows(ft_summary_stat_area %>%
              # remove data that is already in the bd_ft_summary
              filter(!(substring(fishery, 1, 2) %in% c("QO", "QT", "TT", "TR"))) %>%
              group_by(fishery) %>%
              summarise(effort = sum(effort, na.rm = T))) -> total_effort

## fish ticket summary
bd_ft_summary %>%
  rename_all(tolower) %>%
  filter(species == "snow") %>%
  mutate(fishery = case_when(fishery == "snow" ~ paste0("QO", substring(year, 3, 4)),
                             fishery == "Tanner W" ~ paste0("QT", substring(year, 3, 4)),
                             fishery == "Tanner E" ~ paste0("TT", substring(year, 3, 4)),
                             fishery == "BBRKC" ~ paste0("TR", substring(year, 3, 4)))) %>%
  group_by(fishery) %>%
  summarise(retained_lb = sum(ret_cat_lb, na.rm = T),
            retained_num = sum(ret_cat_crabs, na.rm = T),
            effort = sum(fish_effort, na.rm = T)) -> bd_ft_summary

# item 1 ----
## size frequency of retained catch by shell condition in directed snow crab fishery
## shell codes are lumped to broad catagories of "new" and "old"

dock %>%
  # filter for directed fisheries
  filter(substring(fishery, 1, 2) == "QO") %>%
  # generate size comp
  f_retained_size_comp(x = ., lump = T) %>%
  # add a column for total
  mutate(total = rowSums(.[, which(names(.) %in% shell_levels)])) %>%
  arrange(opening_year) %>%
  # save output
  write_csv(here("snow_crab/output", "item1_directed_fishery_dockside_size_comp.csv"))
            
# item 2 ----
## size frequency of snow crab catch in all BSAI crab fisheries
obs_meas %>%
  # generate size composition, lump shell conditions into new and old
  f_observer_size_comp(x = ., by = 3, lump = T) %>%
  # add a column for total (using generalized code to account for any possible shell condition)
  mutate(total = rowSums(.[, which(names(.) %in% shell_levels)])) %>%
  # save output
  write_csv(here("snow_crab/output", "item2_snow_crab_observer_size_comp.csv"))
  
# item 3 ----
## retained catch in numbers and biomass (total for directed fishery)

bd_ft_summary %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "QO") %>%
  # add retained catch in tons
  mutate(retained_t = retained_lb * 0.000453592) %>%
  dplyr::select(fishery, retained_lb, retained_t, retained_num, effort) %>%
  rename(total_potlifts = effort) %>%
  # uncode fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  arrange(opening_year) %>%
  # save output
  write_csv(here("snow_crab/output", "item3_snow_crab_retained_catch_directed_fishery.csv"))



# item 4 ----
## total catch of male and female snow crab in BSAI crab fisheries

### prepare avg wt for biomass expansion
obs_meas %>%
  # get average weights for biomass expansion
  f_average_wt(., by = 4, units = "lbs") %>%
  # add legal status text that matches pot sum groups
  mutate(group = case_when((sex == 2) ~ "female",
                           (sex == 1 & legal_status == F) ~ "sublegal",
                           (sex == 1 & legal_status == T) ~ "tot_legal")) %>%
  ungroup() %>%
  dplyr::select(fishery, group, avg_wt) -> avg_wt_lbs

### summarise count pot data and expand
pot_sum %>%
  # only keep female, sublegal male, and total legal male counts
  dplyr::select(-legal_ret, -legal_nr, -legal_ur) %>%
  # count the number of unique observer pots
  group_by(fishery) %>%
  mutate(obs_pots = n()) %>%
  # pivot to long format by legal group
  pivot_longer(c("female", "sublegal", "tot_legal"), names_to = "group", values_to = "count") %>%
  # aggregate data by fishery
  group_by(fishery, group) %>%
  summarise(obs_pots = mean(obs_pots),
            count = sum(count, na.rm = T)) %>%
  # compute cpue
  mutate(cpue = count / obs_pots) %>%
  # scale to total effort
  left_join(total_effort, by = "fishery") %>%
  mutate(tot_catch_number = cpue * effort) %>%
  # join to avg_wt_lbs
  left_join(avg_wt_lbs, by = c("fishery", "group")) %>%
  # use timeseries average weight if subgroup was caught in count pots, but not measure pots
  mutate(area = substring(fishery, 1, 2)) %>%
  group_by(area, group) %>%
  mutate(ts_avg_wt = mean(avg_wt, na.rm = T)) %>%
  ungroup() %>%
  mutate(avg_wt = ifelse(count > 0 & is.na(avg_wt), ts_avg_wt, avg_wt)) %>%
  # compute total catch in weight
  mutate(tot_catch_lb = tot_catch_number * avg_wt,
         tot_catch_t = tot_catch_lb * 0.000453592) %>%
  
  ### issue with CK98 and leal males
  # replace missing value with 0
  replace_na(list(tot_catch_number = 0,
                  tot_catch_lb = 0,
                  tot_catch_t = 0)) %>%
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  dplyr::select(1:5, 10, 14:15) %>%
  arrange(management_area, target, opening_year) %>%
  
  # save output
  write_csv(., here("snow_crab/output", "item4_total_catch.csv"))




# item 5 ----
## retained catch in numbers and biomass (by stat area for directed fishery)
ft_summary_stat_area %>%
  filter(substring(fishery, 1, 2) == "QO", 
         !(as.numeric(substring(fishery, 3, 4)) %in% c(85:89))) %>%
  # add retained catch in tons
  mutate(retained_t = retained_lb * 0.000453592) %>%
  dplyr::select(fishery, stat_area, retained_lb, retained_t, retained_num, effort) %>%
  # add fishery description, filter for sow crab directed fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # save output
  write_csv(here("snow_crab/output", "item5_snow_crab_fish_ticket_stat_area.csv"))


