# notes ----
## fishery data request - bbrkc
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 7/6/2020

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season
season <- "2019_20"

# data inputs ----

## dockside data
dock <- read_csv(here("bbrkc/data", "RKC-1990-2019_dockside.csv"))

## observer crab detail data
obs_meas <- read_csv(here("bbrkc/data", "RKC-1990-2019_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("bbrkc/data", "RKC-1990-2019_potsum.csv"))

## fish ticket data by stat area
fish_tick<- read_csv(here("bbrkc/data", "bsai_crab_fish_ticket_summary_stat_area.csv"))

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
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # filter for only fisheries since rationalization
  filter(as.numeric(substring(fishery, 3, 4)) >= 5,
         as.numeric(substring(fishery, 3, 4)) < 80,
         !(fishery %in% c("CO05", "QO05o"))) %>%
  # remove 'r' in QO05 fishery code
  mutate(fishery = gsub("r", "", fishery)) -> obs_meas

pot_sum %>%
  # remove added column start_year
  dplyr::select(-start_year) %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # filter for only fisheries since rationalization
  filter(as.numeric(substring(fishery, 3, 4)) >= 5,
         as.numeric(substring(fishery, 3, 4)) < 80,
         !(fishery %in% c("CO05", "QO05o"))) %>%
  # remove 'r' in QO05 fishery code
  mutate(fishery = gsub("r", "", fishery)) -> pot_sum

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

## get directe effort in the bbrkc fishery
fish_tick %>%
  # combine XR and TR fisheries
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  group_by(fishery) %>%
  summarise_all(sum, na.rm = T) %>%
  ungroup() %>%
  filter(substring(fishery, 1, 2) == "TR") %>%
  dplyr::select(fishery, effort) -> directed_effort

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
  
## comput total catch weight (lbs) with measure pot data
obs_meas %>%
  # remove crab without a legal code
  filter(!is.na(legal)) %>%
  # decipher legal could into legal group
  f_sdr(col = "legal", type = "legal") %>%
  mutate(group = ifelse(grepl("legal_", legal_text), "tot_legal", legal_text)) %>%
  # add maturity based on clutch
  mutate(maturity = case_when(sex == 1 ~ "male",
                              (sex == 2 & clutch == 0) ~ "immature",
                               (sex == 2 & clutch != 0) ~ "mature")) %>%
  # count by fishery, sex, group, size, maturity
  count(fishery, spcode, sex, group, maturity, size) %>%
  # join to length-weight parameters
  left_join(read_csv(here("misc/data", "weight_parameters.csv")),
            by = c("spcode", "sex", "maturity")) %>%
  # compute weight (lbs) in by line combination
  mutate(wt_lbs = (alpha * size^beta) * 0.0022046226218 * n) %>%
  # join to measure pot effort and fishery directed effort
  left_join(measured_effort, by = "fishery") %>%
  left_join(directed_effort, by = "fishery") %>%
  # compute total catch weight by fishery and group
  mutate(total_weight = (wt_lbs / obs_effort) * effort) %>%
  group_by(fishery, group) %>%
  summarise(total_catch_lbs = sum(total_weight, na.rm = T)) %>%
  # join to total catch number
  left_join(total_catch_num, by = c("fishery", "group")) %>%
  # decipher fishery code, filter for most recent directed fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(substring(fishery, 1, 2) == "TR",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  write_csv(here(paste0("bbrkc/output/", season), "item1_total_catch_directed_fishery.csv"))

  
# item 2 ----
## number and weight of rkc caught in the directed tanner crab E166
  
## get direct effort in the bbrkc fishery
fish_tick %>%
  filter(substring(fishery, 1, 2) == "TT") %>%
  dplyr::select(fishery, effort) %>%
  rename(directed_effort = effort) -> directed_effort

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
  left_join(directed_effort, by = "fishery") %>%
  # pivot to long format add sex code
  pivot_longer(c(female, male), names_to = "sex_text", values_to = "count") %>%
  mutate(sex = ifelse(sex_text == "male", 1, 2)) %>%
  # join to average weight by fishery and sex
  left_join(f_average_wt(obs_meas, by = 1), by = c("fishery", "sex")) %>%
  # compute total catch number and weight (lbs)
  mutate(total_catch_num = (count / obs_effort) * directed_effort,
         total_catch_lbs = total_catch_num * avg_wt) %>%
  # remove unneeded data
  dplyr::select(-obs_effort, -directed_effort, -sex, -count, -avg_wt) %>%
  # decipher fishery code and filter for past season
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
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
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4a_sublegal_observer_size_comp_directed_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4b_legal_observer_size_comp_directed_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4c_female_observer_size_comp_directed_fishery.csv"))

# item 5 ----
## size composition of landed catch (dockside sampling) in directed fishery

dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "TR") %>%
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
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "TT") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6a_sublegal_observer_size_comp_tanner_e166_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "TT") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6b_legal_observer_size_comp_tanner_e166_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "TT") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6c_female_observer_size_comp_tanner_e166_fishery.csv"))
