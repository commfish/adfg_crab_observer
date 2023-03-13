# notes ----
## fishery data request - aigkc
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 3/13/2023

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season
season_current <- "2021/22"

# data inputs ----

## dockside data
dock <- read_csv(here("aigkc/data", "AIGKC-1996-2022_retained_size_freq.csv"))

## observer crab detail data
obs_meas <- read_csv(here("aigkc/data", "AIGKC-1996-2022_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("aigkc/data", "AIGKC-1996-2022_potsum.csv"))

## fish tickets by stat area
ft <- f_unpack_fish_ticket_summary("./misc/data/fish_ticket_summaries")

## parameters for calculated weight estimation
params <- read_csv(here("misc/data", "weight_parameters.csv"))

# data management ----

## clean observer and dockside data timeseries
dock %>%
  # combine aigkc tf and directed fishery, adjust codes for tanner e166 fishery
  mutate(fishery = gsub("XE", "OB", fishery)) -> dock


obs_meas %>%
  # combine aigkc tf and directed fishery
  mutate(fishery = gsub("XE", "OB", fishery)) -> obs_meas


pot_sum %>%
  # combine aigkc tf and directed fishery
  mutate(fishery = gsub("XE", "OB", fishery)) -> pot_sum


# item 1 (retained catch) ----

ft %>%
  # combine aigkc tf and directed fishery
  mutate(fishery = gsub("XE", "OB", fishery)) %>%
  # filter for fisheries of interest
  filter(substring(fishery, 1, 2) %in% c("OB", "XE", "RB")) %>%
  # sum number of crab
  group_by(fishery) %>%
  summarise(retained_n = sum(live_number + deadloss_number, na.rm = T),
            retained_lb = sum(live_lbs + deadloss_lbs, na.rm = T),
            retained_t = retained_lb * 0.000453592) %>%
  # save output
  write_csv("aigkc/output/item1_retained_catch.csv")



# item 2 (total directed catch) ----

# get total fishery effort timeseries
ft %>%
  # combine aigkc tf and directed fishery
  mutate(fishery = gsub("XE", "OB", fishery)) %>%
  # filter for fisheries of interest
  filter(substring(fishery, 1, 2) %in% c("OB", "XE", "RB")) %>%
  # sum number of crab
  group_by(fishery) %>%
  summarise(effort = sum(pots, na.rm = T)) %>% ungroup -> directed_effort

## get total catch by legal group in numbers
pot_sum %>%
  # get count of female, sublegal and total legal by fishery, total observer pots
  group_by(fishery) %>%
  summarise(female = sum(female, na.rm = T),
            sublegal = sum(sublegal, na.rm = T),
            tot_legal = sum(tot_legal, na.rm = T),
            obs_effort = n()) %>%
  # pivot to long format
  pivot_longer(c(female, sublegal, tot_legal), names_to = "group", values_to = "count") %>%
  # join to directed fishery effort
  left_join(directed_effort, by = "fishery") %>%
  # compute total catch
  group_by(fishery, group) %>%
  summarise(total_catch_num = (count / obs_effort) * effort) %>%
  ungroup -> total_catch_num
  
## extrapolate to weight using observer measure pot and average wt data
obs_meas %>% 
  filter(!is.na(size), !is.na(sex), sex != -9, size != -9) %>%
  mutate(stock = substring(fishery, 1, 2)) %>%
  f_average_wt(by = 4, units = "lbs") %>%
  # use sex and legal status to assign group
  mutate(group = case_when(sex == 2 & legal_status == F ~ "female",
                           sex == 1 & legal_status == F ~ "sublegal",
                           sex == 1 & legal_status == T ~ "tot_legal")) %>%
  dplyr::select(fishery, group, avg_wt) %>%
  # join to total catch number and extrapolate
  left_join(total_catch_num, by = c("fishery", "group")) %>%
  mutate(total_catch_lbs = total_catch_num * avg_wt,
         total_catch_t = total_catch_lbs * 0.000453592) %>%
  # decipher fishery code, filter for directed fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # correct management_area
  mutate(management_area = ifelse(management_area == "adak", "western aleutians", "eastern aleutians")) %>%
  # remove avg_wt and sex column, and NA groups (sex == 3), arrange by fishery
  ungroup() %>%
  dplyr::select(-avg_wt, -sex) %>%
  filter(!is.na(group)) %>%
  arrange(opening_year) %>%
  write_csv("aigkc/output/item2_directed_total_catch.csv")

# item 3 (retained catch size composition) ----

dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) %in% c("OB", "RB")) %>%
  # save output
  write_csv("aigkc/output/item3_retained_size_comp.csv")


# item 4 (observer size composition) ----

obs_meas %>%
  #filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) %in% c("OB", "RB")) %>%
  # save output
write_csv("aigkc/output/item4_directed_total_size_comp.csv")
