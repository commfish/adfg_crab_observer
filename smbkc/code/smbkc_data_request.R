# notes ----
## fishery data request - smbkc
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 7/13/2020

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season
season <- "2019_20"

# data inputs ----

## dockside data
dock <- read_csv(here("smbkc/data", "BKC-1990-2019_dockside.csv"))

## observer crab detail data
### rkc in king and tanner crab fisheries
obs_meas <- read_csv(here("smbkc/data", "BKC-1990-2019_crab_dump.csv"))

## count pot data
### rkc in king and tanner crab fisheries
pot_sum <- read_csv(here("smbkc/data", "BKC-1990-2019_potsum.csv"))
### remove column start_year(not in standard obs pot summary data format)
pot_sum <- dplyr::select(pot_sum, -start_year)

## fish ticket data by stat area
fish_tick <- read_csv(here("smbkc/data", "bsai_crab_fish_ticket_summary_stat_area.csv"))


# item 1 ----
## total catch from the previous season's fisheries by sex

## get fishery directed effort
fish_tick %>%
  group_by(fishery) %>%
  summarise(directed_effort = sum(effort, na.rm = T)) -> directed_effort

pot_sum %>%
  # sum females, sublegal and legal males by fishery, include observer effort
  group_by(fishery) %>%
  summarise(female = sum(female, na.rm = T),
            sublegal = sum(sublegal, na.rm = T),
            tot_legal = sum(tot_legal, na.rm = T),
            obs_effort = n()) %>%
  # pivot to long format
  pivot_longer(c(female, sublegal, tot_legal), names_to = "group", values_to = "count") %>%
  # join to directed effort
  left_join(directed_effort, by = "fishery") %>%
  # join to average weight by sex
  mutate(sex = ifelse(group == "female", 2, 1)) %>%
  left_join(f_average_wt(x = obs_meas, by = 1, units = "lbs"),
            by = c("fishery", "sex")) %>%
  # compute total catch number and weight
  mutate(total_catch_number = (count / obs_effort) * directed_effort,
         total_catch_lbs = total_catch_number * avg_wt) %>%
  # trim columns, filter for most recent season's fisheries
  dplyr::select(fishery, group, total_catch_number, total_catch_lbs) %>%
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season, "/item1_total_catch_by_fishery.csv")))


# item 2 ----
## fish ticket totals from the previous season's directed fishery

fish_tick %>%
  # sum across stat areas
  group_by(fishery) %>%
  dplyr::select(-stat_area) %>%
  summarise_all(sum, na.rm = T) %>%
  # sum live crab and deadloss
  transmute(fishery = fishery,
            retained_number = live_number + deadloss_number,
            retained_lbs = live_lbs + deadloss_lbs, 
            effort = effort) %>%
  # decipher fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 2) == "QP",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season, "/item2_directed_fishery_stats.csv")))


# item 3 ----
## observer size composition by group in directed fshery

## sublegal males by shell condition
obs_meas %>%
  filter(sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item3a_sublegal_observer_size_comp_smbkc_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item3b_legal_observer_size_comp_smbkc_fishery.csv"))

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item3c_female_observer_size_comp_smbkc_fishery.csv"))

# item 4 ----
## retained catch size composition from directed fishery
dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item4_retained_size_comp.csv"))

# item 5 ----
## observer size composition by group in the snow crab fishery

## sublegal males by shell condition
obs_meas %>%
  filter(sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QO") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item5a_sublegal_observer_size_comp_snow_crab_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QO") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item5b_legal_observer_size_comp_snow_crab_fishery.csv"))

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QO") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item5c_female_observer_size_comp_snow_crab_fishery.csv"))
