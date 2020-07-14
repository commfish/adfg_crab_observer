# notes ----
## fishery data request - pirkc
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 7/10/2020

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season
season <- "2019_20"

# data inputs ----

## dockside data
dock <- read_csv(here("pirkc/data", "RKC-1990-2019_dockside.csv"))

## observer crab detail data
### rkc in king and tanner crab fisheries
obs_meas <- read_csv(here("pirkc/data", "RKC-1990-2019_crab_dump.csv"))
### rkc in snow crab fisheries 
obs_meas_QO <- do.call("rbind", lapply(list.files("./pirkc/data/crab_dump_QO", full.names = T), read_csv))

## count pot data
### rkc in king and tanner crab fisheries
pot_sum <- read_csv(here("pirkc/data", "RKC-1990-2019_potsum.csv"))
### rkc in snow crab fisheries 
pot_sum_QO <- do.call("rbind", lapply(list.files("./pirkc/data/potsum_QO", full.names = T), read_csv))

## fish ticket data by stat area
fish_tick <- read_csv(here("pirkc/data", "bsai_crab_fish_ticket_summary_stat_area.csv"))

# data management ----

## trim columns and join files for measure pot data
obs_meas %>%
  bind_rows(dplyr::select(obs_meas_QO, names(.))) -> obs_meas


## combine pot summary data
## fix column names in snow crab fishery pot summary
names(pot_sum_QO) <- c("fishery", "trip", "adfg", "sampdate", "spn", "statarea", "latitude","longitude", 
                       "e_W", "depth", "soak", "gear", "biotwine", "ring_size", "mesh", "female", "sublegal", 
                       "legal_ret", "legal_nr", "legal_ur", "tot_legal", "msr_pot")
## bind rows
pot_sum %>%
  dplyr::select(-start_year) %>%
  bind_rows(dplyr::select(pot_sum_QO, names(.)[-2])) -> pot_sum


# item 1 ----
## retained catch size composition

dock %>%
  f_retained_size_comp(., lump = F) %>%
  # add row sums
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 2) == "QR",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item1_retained_catch_size_comp.csv")))


# item 2 ----
## observer size composition

obs_meas %>%
  f_observer_size_comp(., by = 2, lump = F) %>%
  # add row sums
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 1) == "Q",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item2_observer_catch_size_comp.csv")))


# item 3 ----
## total retained catch

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
  filter(substring(fishery, 1, 2) == "QR",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item3_total_retained_catch.csv")))


# item 4 ----
## total catch point estimates in all bering sea (Q) fisheries

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
  # trim columns, filter for most recent season's fishery in Bering sea
  dplyr::select(fishery, group, total_catch_number, total_catch_lbs) %>%
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(substring(fishery, 1, 1) == "Q", 
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item4_total_catch_by_fishery.csv")))


# item 5 ----
## fish ticket summary for directed fishery in current season by stat area

fish_tick %>%
  # trim columns
  dplyr::select(-cpue, -avg_wt, -price_lbs) %>%
  # decipher fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 2) == "QR",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item5_fish_ticket_summary_stat_area.csv")))
  
  

