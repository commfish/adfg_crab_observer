# notes ----
## fishery data request - bbrkc
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 1/5/2020

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season (remove when timeseries is produced for all items)
season <- "2019_20"

# data inputs ----

## dockside data
dock <- read_csv(here("bbrkc/data", "RKC-1990-2020_retained_size_freq.csv"))

## observer crab detail data
obs_meas <- read_csv(here("bbrkc/data", "RKC-1990-2020_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("bbrkc/data", "RKC-1990-2020_potsum.csv"))

## fish tickets by stat area
ft <- f_unpack_fish_ticket_summary("./misc/data/fish_ticket_summaries")

## timerseries of directed effort
dir_effort <- read_csv(here("bbrkc/data", "directed_effort_timeseries_DP.csv"))

## parameters for calculated weight estimation
params <- read_csv(here("misc/data", "weight_parameters.csv"))

# data management ----

## fishery codes for early 90s tanner e166 fisheries
early_90s_tt <- c("EI91", "EI92", paste0("QT", 93:96))

## clean observer and dockside data timeseries
dock %>%
  # combine bbrkc tf and directed fishery, adjust codes for tanner e166 fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) -> dock

obs_meas %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
  # filter EI and QT fisheries in early 90s by stat areas e166
  filter(!(fishery %in% early_90s_tt & (statarea > 660000 | statarea < 0))) %>%
  # combine all tanner e166 fishery codes
  mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery)) -> obs_meas

pot_sum %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
  # filter EI and QT fisheries in early 90s by stat areas e166
  filter(!(fishery %in% early_90s_tt & (statarea > 660000 | statarea < 0))) %>%
  # combine all tanner e166 fishery codes
  mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery)) -> pot_sum

## summarise fish ticket data by fishery
ft %>%
  dplyr::select(-stat_area, -cpue, -avg_wt, -price_per_lb) %>%
  group_by(fishery) %>%
  summarise_all(sum, na.rm = T) -> fish_tick_summary


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
  summarise(female = paste(sum(female, na.rm = T), var(female) / n()),
            sublegal = paste(sum(sublegal, na.rm = T), var(sublegal) / n()),
            tot_legal = paste(sum(tot_legal, na.rm = T), var(tot_legal) / n()),
            obs_effort = n()) %>%
  # pivot to long format
  pivot_longer(c(female, sublegal, tot_legal), names_to = "group", values_to = "count") %>%
  separate(count, into = c("count", "var"), convert = T, sep = " ") %>%
  # join to directed fishery effort
  left_join(directed_effort, by = "fishery") %>%
  # compute total catch
  group_by(fishery, group) %>%
  summarise(total_catch_num = (count / obs_effort) * effort,
            total_catch_se = sqrt(var * effort^2),
            total_catch_cv = total_catch_se / total_catch_num) %>%
  ungroup -> total_catch_num

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
  write_csv("bbrkc/output/item1_total_catch_directed_fishery_table.csv")
  
# item 2 ----
## number and weight of rkc caught in the directed tanner crab E166
  
## get direct effort in the tanner e166 fishery
dir_effort %>%
  filter(substring(fishery, 1, 2) == "TT") %>%
  # change directed effort (illegal) in 2017 to 0 (no RKC caught)
  mutate(effort = ifelse(fishery == "TT17", 0, effort)) -> directed_effort

## estimate total bycatch
pot_sum %>%
  # filter for directed E166 tanner crab fisheries
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
  write_csv("bbrkc/output/item2_total_catch_tanner_crab_e166_table.csv")
  

# item 3 ----

# timeseries of retained catch in numbers and weight for TR and XR fisheries
ft %>%
  filter(substring(fishery, 1, 2) %in% c("TR", "XR")) %>%
  f_sdr(col = "fishery", type = "fishery_code") %>%
  group_by(opening_year) %>%
  summarize(retained_number = sum(live_number, na.rm = T) + sum(deadloss_number, na.rm = T),
            retained_lbs = sum(live_lbs, na.rm = T) + sum(deadloss_lbs, na.rm = T),
            retained_t = retained_lbs * 0.000453592) %>%
  write_csv("bbrkc/output/item3_retained_catch_table.csv")

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
  write_csv("bbrkc/output/item4a_sublegal_observer_size_comp_directed_fishery_table.csv")

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv("bbrkc/output/item4b_legal_observer_size_comp_directed_fishery_table.csv")

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv("bbrkc/output/item4c_female_observer_size_comp_directed_fishery_table.csv")

# item 5 ----
## size composition of landed catch (dockside sampling) in directed fishery

dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv("bbrkc/output/item5_retained_size_comp_table.csv")
  
  

# item 6 ----
## observer size composition, by legal status and shell condition from tanner crab e166 fishery

## sublegal males by shell condition
obs_meas %>%
  # filter for directed E166 tanner crab fisheries
  filter(substring(fishery, 1, 2) == "TT",
         sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # save output
  write_csv("bbrkc/output/item6a_sublegal_observer_size_comp_tanner_e166_fishery_table.csv")

## all legal males by shell condition
obs_meas %>%
  # filter for directed E166 tanner crab fisheries
  filter(substring(fishery, 1, 2) == "TT",
         sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # save output
  write_csv("bbrkc/output/item6b_legal_observer_size_comp_tanner_e166_fishery_table.csv")

## females by shell condition
obs_meas %>%
  # filter for directed E166 tanner crab fisheries
  filter(substring(fishery, 1, 2) == "TT",
         sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # save output
  write_csv("bbrkc/output/item6c_female_observer_size_comp_tanner_e166_fishery_table.csv")
