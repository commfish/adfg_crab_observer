# notes ----
## fishery data request - bbrkc
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 12/9/2020

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season (remove when timeseries is produced for all items)
season <- "2019_20"

# data inputs ----

## dockside data
dock <- read_csv(here("bbrkc/data", "RKC-1990-2019_dockside.csv"))

## observer crab detail data
obs_meas <- read_csv(here("bbrkc/data", "RKC-1990-2019_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("bbrkc/data", "RKC-1990-2019_potsum.csv"))

## fish ticket data by stat area
fish_tick <- read_csv(here("bbrkc/data", "bsai_crab_fish_ticket_summary_stat_area.csv"))

## timerseries of directed effort
dir_effort <- read_csv(here("bbrkc/data", "directed_effort_timeseries_DP.csv"))

## parameters for calculated weight estimation
params <- read_csv(here("misc/data", "weight_parameters.csv"))

# data management ----

## clean observer and dockside data timeseries

dock %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # filter for only fisheries since rationalization
  filter(as.numeric(substring(fishery, 3, 4)) >= 5,
         as.numeric(substring(fishery, 3, 4)) < 80,
         !(fishery %in% c("CO05", "QO05o"))) %>%
  # remove 'r' in QO05 fishery code
  mutate(fishery = gsub("r", "", fishery)) -> dock

obs_meas %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR", "TR", fishery)) -> obs_meas

pot_sum %>%
  # remove added column start_year
  dplyr::select(-start_year) %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR", "TR", fishery)) -> pot_sum

## summarise fish ticket data by fishery
fish_tick %>%
  dplyr::select(-stat_area, -cpue, -avg_wt, -price_lbs) %>%
  group_by(fishery) %>%
  summarise_all(sum, na.rm = T) -> fish_tick


# item 1 ----
## total catch of sublegal males and females in the most recent directed 
## RKC fishery and test fishery

## get observer measure pot effort 
pot_sum %>%
  # filter for measured pots in bbrkc directed fisheries
  filter(substring(fishery, 1, 2) == "TR",
         msr_pot == "Y") %>%
  group_by(fishery) %>%
  summarise(obs_effort = n()) -> measured_effort

# get directed effort in the bbrkc fishery
dir_effort %>%
  filter(substring(fishery, 1, 2) == "TR") -> directed_effort

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
  summarise(total_catch_num = (count / obs_effort) * effort) -> total_catch_num

## extrapolate to weight using observer measure pot and average wt data
obs_meas %>%
  f_average_wt(by = 4, units = "lbs") %>%
  # use sex and legal status to assign group
  mutate(group = case_when(sex == 2 & legal_status == F ~ "female",
                           sex == 1 & legal_status == F ~ "sublegal",
                           sex == 1 & legal_status == T ~ "tot_legal")) %>%
  dplyr::select(fishery, group, avg_wt) %>%
  # join to total catch number and extrapolate
  left_join(total_catch_num, by = c("fishery", "group")) %>%
  mutate(total_catch_lbs = total_catch_num * avg_wt) %>%
  # decipher fishery code, filter for directed fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(substring(fishery, 1, 2) == "TR") %>%
  # remove avg_wt and sex column, and NA groups (sex == 3), arrange by fishery
  ungroup() %>%
  dplyr::select(-avg_wt, -sex) %>%
  filter(!is.na(group)) %>%
  arrange(opening_year) %>%
  write_csv(here(paste0("bbrkc/output/", season), "item1_total_catch_directed_fishery.csv"))
  
# item 2 ----
## number and weight of rkc caught in the directed tanner crab E166
  
## get direct effort in the tanner e166 fishery
dir_effort %>%
  filter(substring(fishery, 1, 2) == "TT") -> directed_effort

## estimate total bycatch
pot_sum %>%
  # filter for direct E166 tanner crab fisheries
  filter(substring(fishery, 1, 2) == "TT") %>%
  # summarise number of crab caught by sex
  group_by(fishery) %>%
  summarise(female = sum(female, na.rm = T),
            male = sum(sublegal, tot_legal, na.rm = T),
            obs_effort = n()) %>%
  # join to observer effort
  right_join(directed_effort, by = "fishery") %>%
  # pivot to long format add sex code
  pivot_longer(c(female, male), names_to = "sex_text", values_to = "count") %>%
  mutate(sex = ifelse(sex_text == "male", 1, 2)) %>%
  # join to average weight by fishery and sex
  left_join(f_average_wt(obs_meas, by = 1, units = "lbs"), by = c("fishery", "sex")) %>%
  # compute total catch number and weight (lbs)
  mutate(total_catch_num = (count / obs_effort) * effort,
         total_catch_lbs = total_catch_num * avg_wt) %>%
  # replace missing estimates with zeros (closed fishery)
  replace_na(list(total_catch_num = 0,
                  total_catch_lbs = 0)) %>%
  # remove unneeded data
  dplyr::select(-obs_effort, -effort, -sex, -count, -avg_wt) %>%
  # decipher fishery code and filter for past season, arrange by season
  f_sdr(col = "fishery", type = "fishery_code") %>%
  arrange(opening_year) %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item2_total_catch_tanner_crab_e166.csv"))
  

# item 3 ----
## fish ticket summary by fishery, TR and XR fisheries separate
fish_tick %>%
  # filter for bbrkc directed and cost recovery fishery
  filter(substring(fishery, 1, 2) %in% c("TR", "XR")) %>%
  # decihper fishery code and filter for most recent season
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item3_fish_ticket_summary.csv"))


# item 4 ----
## observer size composition, by legal status and shell condition from bbrkc directed fishery

## sublegal males by shell condition
obs_meas %>%
  filter(sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4a_sublegal_observer_size_comp_directed_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4b_legal_observer_size_comp_directed_fishery.csv"))

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4c_female_observer_size_comp_directed_fishery.csv"))

# item 5 ----
## size composition of landed catch (dockside sampling) in directed fishery

dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item5_retained_size_comp.csv"))
  
  

# item 6 ----
## observer size composition, by legal status and shell condition from tanner crab e166 fishery

## sublegal males by shell condition
obs_meas %>%
  filter(sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for filter for tanner e166 fisheries
  filter(substring(fishery, 1, 2) == "TT") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6a_sublegal_observer_size_comp_tanner_e166_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for filter for tanner e166 fisheries
  filter(substring(fishery, 1, 2) == "TT") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6b_legal_observer_size_comp_tanner_e166_fishery.csv"))

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for filter for tanner e166 fisheries
  filter(substring(fishery, 1, 2) == "TT") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6c_female_observer_size_comp_tanner_e166_fishery.csv"))

