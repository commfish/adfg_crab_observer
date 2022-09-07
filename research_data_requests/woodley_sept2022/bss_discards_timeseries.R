# notes ---- 
# snow crab discard estimate timeseries
# tyler jackson
# 9/7/2022


# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

# data inputs ----

## dockside data
dock <- read_csv("./snow_crab/data/SNOWCRAB-1990-2021_retained_size_freq.csv")

## observer crab detail data
obs_meas <- read_csv("./snow_crab/data/SNOWCRAB-1990-2021_crab_dump.csv")

## count pot data
pot_sum <- read_csv("./snow_crab/data/SNOWCRAB-1990-2021_potsum.csv")

## ben's stat area fish ticket summary for all fisheries directed catch
bd_ft_summary <- read_csv("./snow_crab/data/retained_catch_all_fisheries.csv") 

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
  summarise(effort = sum(fish_effort, na.rm = T)) -> total_effort

## retained catch 
bd_ft_summary %>%
  rename_all(tolower) %>%
  filter(dir_inc == "directed") %>%
  mutate(fishery = case_when(fishery == "snow" ~ paste0("QO", substring(year, 3, 4)),
                             fishery == "Tanner W" ~ paste0("QT", substring(year, 3, 4)),
                             fishery == "Tanner E" ~ paste0("TT", substring(year, 3, 4)),
                             fishery == "BBRKC" ~ paste0("TR", substring(year, 3, 4)))) %>%
  group_by(fishery) %>%
  summarise(ret_cat_crabs = sum(ret_cat_crabs, na.rm = T),
            ret_cat_lb = sum(ret_cat_lb, na.rm = T)) -> total_retained

# total catch estimation ----

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
  # filter for directed fisheries
  filter(substring(fishery, 1, 2) == "QO") %>%
  dplyr::select(fishery, group, effort, tot_catch_number, tot_catch_lb) %>%
  f_sdr(., col = "fishery", type = "fishery_code") -> total_catch

# discard estimation ----

### female and sublegals are total catch
### legal males by subtraction method
total_catch %>%
  left_join(total_retained) %>%
  mutate(discards = case_when(group %in% c("female", "sublegal") ~ tot_catch_number,
                              group == "tot_legal" ~ tot_catch_number - ret_cat_crabs),
         discard_lb = case_when(group %in% c("female", "sublegal") ~ tot_catch_lb,
                                group == "tot_legal" ~ tot_catch_lb - ret_cat_lb)) %>%
  dplyr::select(fishery, opening_year, group, discards, discard_lb) -> discard_est

### outputs
#### lb
discard_est %>%
  dplyr::select(-discards) %>%
  pivot_wider(names_from = group, values_from = discard_lb) %>%
  write_csv("./research_data_requests/woodley_sept2022/bss_discards_lb.csv")
#### number
discard_est %>%
  dplyr::select(-discard_lb) %>%
  pivot_wider(names_from = group, values_from = discards) %>%
  write_csv("./research_data_requests/woodley_sept2022/bss_discards_number.csv")





