# notes ----
## fishery data request - bbrkc
## output files format - gmacs .dat file
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 12/9/2020

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season (remove when timeseries is produced for all items)
season <- "2019_20"

## bin vector for size comps
sizebin_vector <- seq(70, 160, 5)

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
         seas = 3, 
         fleet = 1, 
         sex = 1,
         cv = 0.04, 
         type = 0, 
         units = 1, 
         mult = 1,
         effort = 0,
         discard_mortality = 0.2) %>%
  # re-order, comment out header, and save .txt file
  dplyr::select(year, seas, fleet, sex, obs, cv, type, units, mult, effort, discard_mortality) %>%
  rename(`#year` = opening_year) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item1a_total_catch_directed_fishery_males.txt"), delim = "\t")

## female discards (t) in directed fishery (gmacs format)
dir_catch_est %>%
  filter(group == c("female")) %>%
  group_by(opening_year) %>%
  summarise(obs = sprintf('%.1f', sum(total_catch_t))) %>%
  # add columsn requiresd by gmacs
  mutate(year = opening_year,
         seas = 3, 
         fleet = 1, 
         sex = 2,
         cv = 0.07, 
         type = 0, 
         units = 1, 
         mult = 1,
         effort = 0,
         discard_mortality = 0.2) %>%
  # re-order, comment out header, and save .txt file
  dplyr::select(year, seas, fleet, sex, obs, cv, type, units, mult, effort, discard_mortality) %>%
  rename(`#year` = opening_year) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item1b_total_catch_directed_fishery_females.txt"), delim = "\t")
  


  
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
  left_join(f_average_wt(obs_meas, by = 1, units = "kg"), by = c("fishery", "sex")) %>%
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
         seas = 5, 
         fleet = 3, 
         sex = 1,
         cv = 0.07, 
         type = 2, 
         units = 1, 
         mult = 1,
         potlifts = effort / 1000,
         discard_mortality = 0.25) %>%
  # re-order and save .txt file
  dplyr::select(year, seas, fleet, sex, obs, cv, type, units, mult, potlifts, discard_mortality) %>%
  # add small modifier to avoid zero pot lifts
  # comment out years when the fishery is closed
  mutate(potlifts = ifelse(potlifts == 0, 0.0001, potlifts),
         year = ifelse(potlifts == 0.0001, paste0("#", year), year)) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item2a_tanner_e166_bycatch_males.txt"), delim = "\t")

## total female catch (t) in tanner crab fishery e166 (gmacs format)
crab_fishery_bycatch_est %>%
  filter(sex_text == "male") %>%
  mutate(obs = sprintf('%.3f', total_catch_t)) %>%
  mutate(year = opening_year, 
         seas = 5, 
         fleet = 3, 
         sex = 2,
         cv = 0.07, 
         type = 2, 
         units = 1, 
         mult = 1,
         potlifts = effort / 1000,
         discard_mortality = 0.25) %>%
  # re-order and save .txt file
  dplyr::select(year, seas, fleet, sex, obs, cv, type, units, mult, potlifts, discard_mortality) %>%
  # add small modifier to avoid zero pot lifts
  # comment out years when the fishery is closed
  mutate(potlifts = ifelse(potlifts == 0, 0.0001, potlifts),
         year = ifelse(potlifts == 0.0001, paste0("#", year), year)) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item2b_tanner_e166_bycatch_female.txt"), delim = "\t")


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
## observer size composition, by sex from bbrkc directed fishery

## total males
obs_meas %>%
  filter(sex == 1, legal %in% c(0, 1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[7:ncol(.)]),
         size_bin = cut(size, 
                        breaks = c(0, sizebin_vector, Inf),
                        labels = paste0("cl", seq(67.5, 162.5, 5)))) %>%
  # filter for most recent directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # sum across size bins, normalize 0 - 1
  group_by(opening_year, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  group_by(opening_year) %>%
  mutate(prop =  sprintf('%.4f', total / sum(total))) %>%
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
         maturity = 0,
         nsamp = 100) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("cl", names(.), value = T)) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 19)), .) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item4a_directed_fishery_size_comp_total_males.txt"), delim = "\t", col_names = F, na = "")

## total females
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[7:ncol(.)]),
         size_bin = cut(size, 
                        breaks = c(0, sizebin_vector[-c(16:20)], Inf),
                        labels = paste0("cl", seq(67.5, 142.5, 5)))) %>%
  # filter for most recent directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # sum across size bins, normalize 0 - 1
  group_by(opening_year, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  group_by(opening_year) %>%
  mutate(prop =  sprintf('%.4f', total / sum(total))) %>%
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") %>%
  # additional data
  mutate(year = opening_year,
         season = 3, 
         fleet = 1, 
         sex = 2,
         type = 0,
         shell = 0,
         maturity = 0,
         nsamp = 50) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("cl", names(.), value = T)) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 15)), .) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item4b_directed_fishery_size_comp_total_females.txt"), delim = "\t", col_names = F, na = "")




# item 5 ----
## size composition of landed catch (dockside sampling) in directed fishery

dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[6:ncol(.)]),
         size_bin = cut(size, 
                        breaks = c(0, sizebin_vector, Inf),
                        labels = paste0("cl", seq(67.5, 162.5, 5)))) %>%
  # filter for most recent directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # sum across size bins, normalize to 1
  group_by(opening_year, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  group_by(opening_year) %>%
  mutate(prop =  sprintf('%.4f', total / sum(total))) %>%
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") %>%
  # additional data
  mutate(year = opening_year,
         season = 3, 
         fleet = 1, 
         sex = 1,
         type = 1,
         shell = 0,
         maturity = 0,
         nsamp = 100) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("cl", names(.), value = T)) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 19)), .) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item5_directed_fishery_size_comp_retained_males.txt"), delim = "\t", col_names = F, na = "") 
  
  


# item 6 ----
## observer size composition, by legal status and shell condition from tanner crab e166 fishery
obs_meas %>%
  filter(legal %in% c(-7, 0, 1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total, size bin
  mutate(total = rowSums(.[7:ncol(.)]),
         size_bin = cut(size, 
                        breaks = c(0, sizebin_vector, Inf),
                        labels = paste0("cl", seq(67.5, 162.5, 5)))) %>%
  # filter for tanner e166 fisheries
  filter(substring(fishery, 1, 2) == "TT") %>%
  # sum across size bins, normalize to 1 across both sexes
  group_by(opening_year, sex, size_bin, .drop = F) %>%
  summarise(total = sum(total, na.rm = T)) %>%
  group_by(opening_year) %>%
  mutate(prop =  sprintf('%.4f', total / sum(total))) %>%
  ungroup() %>%
  # pivot to wide format
  dplyr::select(-total) %>%
  pivot_wider(names_from = "size_bin", values_from = "prop") -> tt_size_comp

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
         maturity = 0,
         nsamp = 50) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("cl", names(.), value = T)) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 19)), .) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item6a_tanner_e166_size_comp_males.txt"), delim = "\t", col_names = F, na = "")

# save male matrix
tt_size_comp %>%
  filter(sex == "female") %>%
  # combine last five bins to one
  mutate_at(18:22, as.numeric) %>%
  mutate(cl142.5 =  sprintf('%.4f', cl142.5 + cl147.5 + cl152.5 + cl157.5 + cl162.5)) %>%
  dplyr::select(-cl147.5, -cl152.5, -cl157.5, -cl162.5) %>%
  # additional data
  mutate(year = opening_year,
         season = 5, 
         fleet = 1, 
         sex = 2,
         type = 0,
         shell = 0,
         maturity = 0,
         nsamp = 0) %>%
  dplyr::select(year, season, fleet, sex, type, shell, maturity, nsamp, grep("cl", names(.), value = T)) %>%
  rbind(c("#year", "season", "fleet", "sex", "type", "shell", "maturity", "nsamp", "datavector", rep(NA, 15)), .) %>%
  write_delim(here(paste0("bbrkc/output/", season), "item6b_tanner_e166_size_comp_females.txt"), delim = "\t", col_names = F, na = "")

  
  
 