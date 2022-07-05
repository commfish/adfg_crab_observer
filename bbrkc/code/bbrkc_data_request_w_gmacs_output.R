# notes ----
## fishery data request - bbrkc
## output files format - gmacs .dat file
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 6/25/2021

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## bin vector for size comps
sizebin_vector <- seq(70, 160, 5)

# data inputs ----

## dockside data
dock <- read_csv(here("bbrkc/data", "RKC-1990-2020_retained_size_freq.csv"))

## observer crab detail data
obs_meas <- read_csv(here("bbrkc/data", "RKC-1990-2020_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("bbrkc/data", "RKC-1990-2020_potsum.csv"))

## fish ticket data by stat area
# ft_files <- list.files(here("misc/data/fish_ticket_summaries"), full.names = T)
# c(lapply(grep(".xlsx", ft_files, value = T), f_read_fish_tick_xlsx),
#   lapply(ft_files[!grepl(".xlsx", ft_files)], f_read_fish_tick_xlsx, format = "old")) %>%
#   do.call("rbind", .) -> fish_tick

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

pot_sum %>%
  # fix biotwine status data
  mutate(biotwine_ok = case_when(biotwine_ok == "-" ~ NA,
                                 biotwine_ok %in% c("n", "N") ~ F,
                                 biotwine_ok %in% c("y", "Y") ~ T)) %>%
  # remove added column start_year
  dplyr::select(-start_year) %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
  # filter EI and QT fisheries in early 90s by stat areas e166
  filter(!(fishery %in% early_90s_tt & (statarea > 660000 | statarea < 0))) %>%
  # combine all tanner e166 fishery codes
  mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery)) -> pot_sum

obs_meas %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
  # filter EI and QT fisheries in early 90s by stat areas e166
  filter(!(fishery %in% early_90s_tt & (statarea > 660000 | statarea < 0))) %>%
  # combine all tanner e166 fishery codes
  mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery)) -> obs_meas

## summarise fish ticket data by fishery
# fish_tick %>%
#   dplyr::select(-stat_area, -cpue, -avg_wt, -price_lbs) %>%
#   group_by(fishery) %>%
#   summarise_all(sum, na.rm = T) -> fish_tick_summary

# item 1 ----
## total catch of sublegal males and females
## RKC fishery and test fishery

## get observer measure pot effort 
pot_sum %>%
  # filter for measured pots in bbrkc directed fisheries
  # filter out bad biotwine pots
  filter(substring(fishery, 1, 2) == "TR",
         msr_pot == "Y",
         (biotwine_ok == T | is.na(biotwine_ok))) %>%
  group_by(fishery) %>%
  summarise(obs_effort = n()) -> measured_effort

# get directed effort in the bbrkc fishery
dir_effort %>%
  filter(substring(fishery, 1, 2) == "TR") -> directed_effort

## get total catch by legal group in numbers
pot_sum %>%
  # filter out bad biotwine pots
  filter((biotwine_ok == T | is.na(biotwine_ok))) %>%
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
  # # add and filter for biotwine status
  # left_join(pot_sum %>%
  #             dplyr::select(fishery, trip, adfg, sampdate, spn),
  #           by = c("fishery", "trip", "adfg", "sampdate", "spn")) %>%
  filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
  # compute average individual weight
  f_average_wt(by = 4, units = "kg") %>%
  # use sex and legal status to assign group
  mutate(group = case_when(sex == 2 & legal_status == F ~ "female",
                           sex == 1 & legal_status == F ~ "sublegal",
                           sex == 1 & legal_status == T ~ "tot_legal")) %>%
  dplyr::select(fishery, group, avg_wt) %>%
  # join to total catch number and extrapolate
  left_join(total_catch_num, by = c("fishery", "group")) %>%
  mutate(total_catch_t = total_catch_num * avg_wt / 1000) %>%
  # decipher fishery code, filter for directed fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(substring(fishery, 1, 2) == "TR") %>%
  # remove avg_wt and sex column, and NA groups (sex == 3), arrange by fishery
  ungroup() %>%
  dplyr::select(-avg_wt, -sex) %>%
  filter(!is.na(group)) %>%
  arrange(opening_year) -> dir_catch_est

## total male catch (t) in directed fishery (gmacs format)
dir_catch_est %>%
  filter(group %in% c("sublegal", "tot_legal")) %>%
  group_by(opening_year) %>%
  summarise(obs = sprintf('%.1f', sum(total_catch_t))) %>% 
  # add columsn required by gmacs
  mutate(year = opening_year, 
         season = 3, 
         fleet = 1, 
         sex = 1,
         cv = 0.04, 
         type = 0, 
         units = 1, 
         mult = 1,
         effort = 0,
         discard_mortality = 0.2) %>%
  # re-order, comment out header, and save .txt file
  dplyr::select(year, season, fleet, sex, obs, cv, type, units, mult, effort, discard_mortality) %>%
  rename(`#year` = year) %>%
  write_delim(here("bbrkc/output", "item1a_total_catch_directed_fishery_males.txt"), delim = "\t")

## female discards (t) in directed fishery (gmacs format)
dir_catch_est %>%
  filter(group == c("female")) %>%
  group_by(opening_year) %>%
  summarise(obs = sprintf('%.1f', sum(total_catch_t))) %>%
  # add columsn requiresd by gmacs
  mutate(year = opening_year,
         season = 3, 
         fleet = 1, 
         sex = 2,
         cv = 0.07, 
         type = 0, 
         units = 1, 
         mult = 1,
         effort = 0,
         discard_mortality = 0.2) %>%
  # re-order, comment out header, and save .txt file
  dplyr::select(year, season, fleet, sex, obs, cv, type, units, mult, effort, discard_mortality) %>%
  rename(`#year` = year) %>%
  write_delim(here("bbrkc/output", "item1b_total_catch_directed_fishery_females.txt"), delim = "\t")
  
  
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
  filter(substring(fishery, 1, 2) == "TT",
         (biotwine_ok == T | is.na(biotwine_ok))) %>%
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
  left_join(f_average_wt(obs_meas %>%
                           # left_join(pot_sum %>%
                           #             dplyr::select(fishery, trip, adfg, sampdate, spn),
                           #           by = c("fishery", "trip", "adfg", "sampdate", "spn")) %>%
                           filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))), 
                         by = 1, units = "kg"), by = c("fishery", "sex")) %>%
  # compute total catch number and weight (t)
  mutate(total_catch_num = (count / obs_effort) * effort,
         total_catch_t = total_catch_num * avg_wt / 1000) %>%
  # replace missing estimates with zeros (closed fishery)
  replace_na(list(total_catch_num = 0,
                  total_catch_t = 0)) %>%
  # remove unneeded data
  dplyr::select(-obs_effort, -sex, -count, -avg_wt) %>%
  # decipher fishery code and filter for past season, arrange by season
  f_sdr(col = "fishery", type = "fishery_code") %>%
  arrange(opening_year) -> crab_fishery_bycatch_est

## total male catch (t) in tanner crab fishery e166 (gmacs format)
crab_fishery_bycatch_est %>%
  filter(sex_text == "male") %>%
  mutate(obs = sprintf('%.3f', total_catch_t)) %>%
  mutate(year = opening_year, 
         season = 5, 
         fleet = 3, 
         sex = 1,
         cv = 0.07, 
         type = 2, 
         units = 1, 
         mult = 1,
         potlifts = effort / 1000,
         discard_mortality = 0.25) %>%
  # re-order and save .txt file
  dplyr::select(year, season, fleet, sex, obs, cv, type, units, mult, potlifts, discard_mortality) %>%
  # add small modifier to avoid zero pot lifts
  # comment out years when the fishery is closed
  mutate(potlifts = ifelse(potlifts == 0, 0.0001, potlifts),
         year = ifelse(potlifts == 0.0001, paste0("#", year), year)) %>%
  write_delim(here("bbrkc/output", "item2a_tanner_e166_bycatch_males.txt"), delim = "\t")

## total female catch (t) in tanner crab fishery e166 (gmacs format)
crab_fishery_bycatch_est %>%
  filter(sex_text == "female") %>%
  mutate(obs = sprintf('%.3f', total_catch_t)) %>%
  mutate(year = opening_year, 
         season = 5, 
         fleet = 3, 
         sex = 2,
         cv = 0.07, 
         type = 2, 
         units = 1, 
         mult = 1,
         potlifts = effort / 1000,
         discard_mortality = 0.25) %>%
  # re-order and save .txt file
  dplyr::select(year, season, fleet, sex, obs, cv, type, units, mult, potlifts, discard_mortality) %>%
  # add small modifier to avoid zero pot lifts
  # comment out years when the fishery is closed
  mutate(potlifts = ifelse(potlifts == 0, 0.0001, potlifts),
         year = ifelse(potlifts == 0.0001, paste0("#", year), year)) %>%
  write_delim(here("bbrkc/output", "item2b_tanner_e166_bycatch_female.txt"), delim = "\t")


# item 3 ----
## fish ticket summary by fishery, TR and XR fisheries separate
# fish_tick_summary %>%
#   # filter for bbrkc directed and cost recovery fishery
#   filter(substring(fishery, 1, 2) %in% c("TR", "XR")) %>%
#   # decihper fishery code and filter for most recent season
#   f_sdr(col = "fishery", type = "fishery_code") %>%
#   filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
#   # save output
#   write_csv(here("bbrkc/output", "item3_fish_ticket_summary.csv"))

# item 4 ----
## observer size composition, by sex from bbrkc directed fishery

## total males
obs_meas %>%
  # # remove failed biotwine pots
  # left_join(pot_sum %>%
  #             dplyr::select(fishery, trip, adfg, sampdate, spn),
  #           by = c("fishery", "trip", "adfg", "sampdate", "spn")) %>%
  filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
  # filter males of appropriate size
  filter(sex == 1, 
         legal %in% c(0, 1, 2, 3, 6),
         size >= 65) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[7:ncol(.)]),
         size_bin = ifelse(size > 160, 160, floor(size/ 5) * 5)) %>%
  # filter for directed fisheries
  filter(substring(fishery, 1, 2) == "TR") %>%
  # sum across size bins
  group_by(fishery, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  # pull in missing bins
  full_join(expand_grid(size_bin = seq(65, 160, by = 5),
                        fishery = filter(obs_meas, substring(fishery, 1, 2) == "TR") %>% 
                          pull(fishery) %>% unique),
            by = c("fishery", "size_bin")) %>%
  replace_na(list(total = 0)) %>%
  # add opening year in again
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # normalise to sum to 1
  group_by(opening_year) %>%
  # compute proportion in size bin and effective number of samples
  mutate(prop =  sprintf('%.4f', total / sum(total)),
         nsamp = min(0.05*sum(total), 100),
         ncrab = paste0("#", sum(total))) %>%
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") %>%
  # additional data
  mutate(year = opening_year,
         season = 3, 
         fleet = 1, 
         sex = 1,
         type = 0,
         shell = 0,
         maturity = 0) %>%
  arrange(year) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("\\d", names(.), value = T), ncrab) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 20)), .) %>%
  write_delim(here("bbrkc/output", "item4a_directed_fishery_size_comp_total_males.txt"), delim = "\t", col_names = F, na = "")

## total females
obs_meas %>%
  # # remove failed biotwine pots
  # left_join(pot_sum %>%
  #             dplyr::select(fishery, trip, adfg, sampdate, spn, biotwine_ok),
  #           by = c("fishery", "trip", "adfg", "sampdate", "spn")) %>%
  filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok))) %>%
  # filter females of appropriate size
  filter(sex == 2,
         size >= 65) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[7:ncol(.)]),
         size_bin = ifelse(size > 140, 140, floor(size/ 5) * 5)) %>%
  # filter for directed fisheries
  filter(substring(fishery, 1, 2) == "TR") %>%
  # sum across size bins
  group_by(fishery, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  # pull in missing bins
  full_join(expand_grid(size_bin = seq(65, 140, by = 5),
                        fishery = filter(obs_meas, substring(fishery, 1, 2) == "TR") %>% pull(fishery) %>% unique),
            by = c("fishery", "size_bin")) %>%
  replace_na(list(total = 0)) %>%
  # add opening year in again
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # normalise to sum to 1
  group_by(opening_year) %>%
  mutate(prop =  sprintf('%.4f', total / sum(total)),
         nsamp = min(0.05*sum(total), 50),
         ncrab = paste0("#", sum(total))) %>%
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop", names_sort = T) %>%
  # additional data
  mutate(year = opening_year,
         season = 3, 
         fleet = 1, 
         sex = 2,
         type = 0,
         shell = 0,
         maturity = 0) %>%
  arrange(year) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("\\d", names(.), value = T), ncrab) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 16)), .) %>%
  write_delim(here("bbrkc/output", "item4b_directed_fishery_size_comp_total_females.txt"), delim = "\t", col_names = F, na = "")




# item 5 ----
## size composition of landed catch (dockside sampling) in directed fishery

dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[6:ncol(.)]),
         size_bin = ifelse(size > 160, 160, floor(size/ 5) * 5)) %>%
  # filter for most recent directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # sum across size bins, normalize to 1
  group_by(fishery, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  # pull in missing bins
  right_join(expand_grid(size_bin = seq(65, 160, by = 5),
                         fishery = filter(dock, substring(fishery, 1, 2) == "TR") %>% pull(fishery) %>% unique),
             by = c("fishery", "size_bin")) %>%
  replace_na(list(total = 0)) %>%
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  group_by(opening_year, fishery) %>%
  mutate(prop =  sprintf('%.4f', total / sum(total)),
         nsamp = min(0.05*sum(total), 100),
         ncrab = sum(total)) %>%
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop", names_sort = T) %>%
  # additional data
  mutate(year = opening_year,
         season = 3, 
         fleet = 1, 
         sex = 1,
         type = 1,
         shell = 0,
         maturity = 0,
         ncrab = paste0("#", ncrab)) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, 
                grep("\\d", names(.), value = T), ncrab) %>%
  arrange(year) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 20)), .) %>%
  write_delim(here("bbrkc/output", "item5_directed_fishery_size_comp_retained_males.txt"), delim = "\t", col_names = F, na = "") 
  
  


# item 6 ----
## observer size composition, by legal status and shell condition from tanner crab e166 fishery
obs_meas %>%
  # # remove failed biotwine pots
  # left_join(pot_sum %>%
  #             dplyr::select(fishery, trip, adfg, sampdate, spn, biotwine_ok),
  #           by = c("fishery", "trip", "adfg", "sampdate", "spn")) %>%
  filter((biotwine_ok %in% c("Y", "-") | is.na(biotwine_ok)),
         legal %in% c(-7, 0, 1, 2, 3, 6),
         substring(fishery, 1, 2) == "TT",
         size >= 65) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[7:ncol(.)]),
         size_bin = ifelse(size > 160, 160, floor(size/ 5) * 5)) %>%
  # filter for tanner e166 fisheries
  filter(substring(fishery, 1, 2) == "TT") %>%
  # sum across size bins
  group_by(fishery, sex, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  # pull in missing bins
  full_join(expand_grid(size_bin = seq(65, 160, by = 5),
                        sex = c("female", "male"),
                        fishery = filter(obs_meas, substring(fishery, 1, 2) == "TT") %>% pull(fishery) %>% unique),
             by = c("fishery", "sex", "size_bin")) %>%
  replace_na(list(total = 0)) %>%
  # add opening year in again
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # normalise to sum to 1 across both sexes
  group_by(opening_year, .drop = F) %>%
  mutate(prop =  sprintf('%.4f', total / sum(total)),
         nsamp = min(0.05*sum(total), 50),
         ncrab = paste0("#", sum(total))) %>%
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop", names_sort = T) -> tt_size_comp

# save male matrix
tt_size_comp %>%
  filter(sex == "male") %>%
  # additional data
  mutate(year = opening_year,
         season = 5, 
         fleet = 1, 
         sex = 1,
         type = 0,
         shell = 0,
         maturity = 0) %>%
  arrange(year) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("\\d", names(.), value = T), ncrab) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 20)), .) %>%
  write_delim(here("bbrkc/output", "item6a_tanner_e166_size_comp_males.txt"), delim = "\t", col_names = F, na = "")

# save female matrix
tt_size_comp %>%
  filter(sex == "female") %>%
  # combine last five bins to one
  mutate_at(23:27, as.numeric) %>%
  mutate(`140` =  sprintf('%.4f', `140` + `145` + `150` + `155` + `160`)) %>%
  dplyr::select(-`145`, -`150`, -`155`, -`160`) %>%
  # additional data
  mutate(year = opening_year,
         season = 5, 
         fleet = 1, 
         sex = 2,
         type = 0,
         shell = 0,
         maturity = 0,
         nsamp = 0) %>%
  arrange(year) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("\\d", names(.), value = T), ncrab) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 16)), .) %>%
  write_delim(here("bbrkc/output", "item6b_tanner_e166_size_comp_females.txt"), delim = "\t", col_names = F, na = "")

  
  
 