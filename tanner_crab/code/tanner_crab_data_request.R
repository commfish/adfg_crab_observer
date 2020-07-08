# notes ----
## fishery data request - tanner crab
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
dock <- read_csv(here("tanner_crab/data", "TANNER-1990-2019_dockside.csv"))

## observer crab detail data
obs_meas <- read_csv(here("tanner_crab/data", "TANNER-1990-2019_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("tanner_crab/data", "TANNER-1990-2019_potsum.csv"))

## trip fish ticket summaries including incidental effort for E/W 166 fisheries
incid_e <- read_csv(here("tanner_crab/data", "ebt_incidental_fish_ticket_report.csv"))
incid_w <- read_csv(here("tanner_crab/data", "wbt_incidental_fish_ticket_report.csv"))
## bbrkc test fishery effort
xr <- read_csv(here("tanner_crab/data", "bbrkc_tf_fish_ticket_effort.csv"))




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
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # filter for only fisheries since rationalization
  filter(as.numeric(substring(fishery, 3, 4)) >= 5,
         as.numeric(substring(fishery, 3, 4)) < 80,
         !(fishery %in% c("CO05", "QO05o"))) %>%
  # remove 'r' in QO05 fishery code
  mutate(fishery = gsub("r", "", fishery)) -> pot_sum

## restructure incidental fish ticket data
incid_e %>%
  # remove rows denoting totals
  filter(!(Seasons %in% grep("Total", Seasons, value = T))) %>%
  # unite columns from the same fishery
  unite("BBR", grep("BBR", names(.))) %>%
  unite("EBT", grep("EBT", names(.))) %>%
  # pivot to long format
  pivot_longer(c("BBR", "EBT"), names_to = "fishery", values_to = "values") %>%
  # add fishery code
  mutate(fishery = case_when(fishery == "BBR" ~ paste0("TR", substring(Seasons, 3, 4)),
                             fishery == "EBT" ~ paste0("TT", substring(Seasons, 3, 4)))) %>%
  dplyr::select(-Seasons, -`ADFG-TripNumber`) %>%
  # separate united data strings
  separate(values, into = c("total_lbs", "total_crab", "total_effort", "directed_lbs", "directed_crab",
                            "directed_effort", "incidental_lbs", "incidental_crab", 
                            "incidental_effort"), sep = "_") %>%
  mutate_at(2:ncol(.), as.numeric) %>%
  # join to bbrkc test fishery effort
  bind_rows(xr %>%
              rename(fishery = fishery_code, 
                     directed_effort = effort)) %>%
  # combine bbrkc directed and test fishery
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # sum across fisheries
  group_by(fishery) %>%
  summarise_all(sum, na.rm = T) %>%
  # remove fisheries pre-reationalization
  filter(as.numeric(substring(fishery, 3, 4)) >= 5,
         as.numeric(substring(fishery, 3, 4)) < 80) -> incid_e
  
incid_w %>%
  # remove rows denoting totals
  filter(!(Seasons %in% grep("Total", Seasons, value = T))) %>%
  # unite columns from the same fishery
  unite("BSS", grep("BSS", names(.))) %>%
  unite("WBT", grep("WBT", names(.))) %>%
  # pivot to long format
  pivot_longer(c("BSS", "WBT"), names_to = "fishery", values_to = "values") %>%
  # add fishery code
  mutate(fishery = case_when(fishery == "BSS" ~ paste0("QO", substring(Seasons, 3, 4)),
                             fishery == "WBT" ~ paste0("QT", substring(Seasons, 3, 4)))) %>%
  dplyr::select(-Seasons, -`ADFG-TripNumber`) %>%
  # separate united data strings
  separate(values, into = c("total_lbs", "total_crab", "total_effort", "directed_lbs", "directed_crab",
                            "directed_effort", "incidental_lbs", "incidental_crab", 
                            "incidental_effort"), sep = "_") %>%
  mutate_at(2:ncol(.), as.numeric) %>%
  # sum across fisheries
  group_by(fishery) %>%
  summarise_all(sum, na.rm = T) %>%
  # remove fisheries pre-reationalization
  filter(as.numeric(substring(fishery, 3, 4)) >= 5,
         as.numeric(substring(fishery, 3, 4)) < 80) -> incid_w

# item 1 ----
## directed effort in the tanner crab, snow crab and bbrkc fisheries

bind_rows(incid_e, incid_w) %>%
  # retain only fishery and directed effort
  dplyr::select(fishery, directed_effort) %>%
  # decipher fishery code
  f_sdr(x = ., col = "fishery", type = "fishery_code") %>%
  # save output
  write_csv(here(paste0("tanner_crab/output/", season), "item1_directed_effort.csv"))
  
# item 2 ----
# extract retained (directed and incidental) catch by fishery/season

bind_rows(incid_e, incid_w) %>%
  # filter for tanner crab fisheries
  filter(grepl("TT|QT", fishery)) %>%
  # remove total columns
  dplyr::select(fishery, directed_lbs, directed_crab, incidental_lbs, incidental_crab) %>%
  # pivot to long format
  unite(c(directed_lbs, directed_crab), col = "directed", sep = "_") %>%
  unite(c(incidental_lbs, incidental_crab), col = "incidental", sep = "_") %>%
  pivot_longer(c(directed, incidental), names_to = "type", values_to = "retained") %>%
  separate(retained, into = c("retained_lbs", "retained_crab"), sep = "_") %>%
  # change fishery to directed fishery and remove 'type' column
  mutate(fishery = ifelse((grepl("TT", fishery) & type == "incidental"), 
                          gsub("TT", "TR", fishery), 
                   ifelse((grepl("QT", fishery) & type == "incidental"),
                          gsub("QT", "QO", fishery), fishery))) %>%
  dplyr::select(-type) %>%
  # uncode fishery
  f_sdr(x = ., col = "fishery", type = "fishery_code") %>%
  arrange(fishery) %>%
  # save output
  write_csv(here(paste0("tanner_crab/output/", season), "item2_retained_catch.csv"))


# item 3 ----
# size composition of retained catch by shell condition, grouped as "new" and "old"

dock %>%
  # compute size comp by lumped shell condition and fishery
  f_retained_size_comp(., lump = T) %>%
  # filter for directed tanner crab fisheries
  filter(target == "tanner_crab") %>%
  # save output
  write_csv(here(paste0("tanner_crab/output/", season), "item3_dockside_size_comp.csv"))

# item 4a ----
# total catch estimation for each fishery by sex and shell condition

## get total observer effort by fishery
pot_sum %>%
  # filter for measure pots only
  filter(msr_pot == "Y") %>%
  group_by(fishery) %>%
  summarise(meas_effort = n()) -> meas_effort

## get total fishery effort by fishery
bind_rows(incid_e, incid_w) %>%
  # retain only fishery and directed effort
  dplyr::select(fishery, directed_effort) -> directed_effort

## estimate total catch
obs_meas %>%
  # get number of crab by fishery, size, sex, shell and maturity (for calc weight)
  dplyr::select(fishery, sex, shell) %>%
  # lump shell condition into "new" (2), "old" (3), and unknown (-9)
  mutate(shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
                                shell %in% c(3:5) ~ 3,
                                is.na(shell) ~ -9,
                                shell == -9 ~ -9)) %>%
  count(fishery, sex, shell_lump) %>%
  rename(count = n) %>%
  # join with observer measure pot effort by fishery
  left_join(meas_effort, by = "fishery") %>%
  # join with total directed effort by fishery
  left_join(directed_effort, by = "fishery") %>%
  # comput total catch by line, in numbers of crab
  mutate(total_catch_num = (count / meas_effort) * directed_effort) %>%
  # join to average weight per group
  left_join(bind_rows(f_average_wt(x = obs_meas, by = 2, units = "lbs"),
                      f_average_wt(x = obs_meas, by = 1, units = "lbs") %>%
                        mutate(shell_lump = -9)),
            by = c("fishery", "sex", "shell_lump")) %>%
  # scale total_catch_num to total_catch_lbs
  mutate(total_catch_lbs = total_catch_num * avg_wt) %>%
  # dicepher sex and shell condition codes
  f_sdr(col = "sex", type = "sex") %>%
  f_sdr(col = "shell_lump", type = "shell_condition") %>%
  # housing keeping task (trim columns, fill NA, and rename)
  dplyr::select(fishery, sex_text, shell_text, total_catch_num, total_catch_lbs) %>%
  replace_na(list(shell_text = "unknown")) %>%
  rename(shell = shell_text,
         sex = sex_text) %>%
  # join to all year and group combinations for full timeseries
  right_join(expand_grid(fishery = directed_effort$fishery,
                         sex = unique(.$sex),
                         shell = unique(.$shell)),
             by = c("fishery", "sex", "shell")) %>%
  replace_na(list(total_catch_num = 0, total_catch_lbs = 0)) %>%
  # decipher fishery code 
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # save output
  write_csv(here(paste0("tanner_crab/output/", season), "item4a_total_catch_sex_shell.csv"))


# item 4b ----
## total catch estimation for sex, legal status, and retained/not retained

pot_sum %>%
  # only keep female, sublegal male, and total legal male counts
  dplyr::select(-legal_ret, -legal_nr, -legal_ur) %>%
  # count the number of unique observer pots
  group_by(fishery) %>%
  mutate(obs_effort = n()) %>%
  # pivot to long format by legal group
  pivot_longer(c("female", "sublegal", "tot_legal"), names_to = "group", values_to = "count") %>%
  # aggregate data by fishery
  group_by(fishery, group) %>%
  summarise(obs_effort = mean(obs_effort),
            count = sum(count, na.rm = T)) %>%
  # scale to total effort
  left_join(directed_effort, by = "fishery") %>%
  mutate(total_catch_num = (count / obs_effort) * directed_effort) %>%
  # join to average weight per group
  left_join(f_average_wt(x = obs_meas, by = 4, units = "lbs") %>%
              mutate(group = case_when(sex == 2 ~ "female",
                                       (sex == 1 & legal_status == T) ~ "tot_legal",
                                       (sex == 1 & legal_status == F) ~ "sublegal")) %>%
              ungroup() %>%
              dplyr::select(-sex, -legal_status),
            by = c("fishery", "group")) %>%
  # scale total_catch_num to total_catch_lbs, save temporary object
  mutate(total_catch_lbs = total_catch_num * avg_wt) -> tmp

# get retained tanner crab catch by fishery (item 2) to comput legal not retained 
# join to tmp file
read_csv(here(paste0("tanner_crab/output/", season), 
              "item2_retained_catch.csv")) -> retained_catch

retained_catch %>%
  dplyr::select(-management_area, -target, -opening_year) %>%
  # join to total catch data
  left_join(tmp %>%
              dplyr::select(fishery, group, total_catch_num, total_catch_lbs) %>%
              filter(group == "tot_legal"),
            by = "fishery") %>%
  replace_na(list(total_catch_num = 0, total_catch_lbs = 0)) %>%
  # calculate legal not retained using subtraction method
  mutate(lnr_num = total_catch_num - retained_crab,
         lnr_lbs = total_catch_lbs - retained_lbs) %>%
  # remove unnecessary data and change group to legal_nr
  dplyr::select(fishery, group, lnr_num, lnr_lbs) %>%
  mutate(group = "legal_nr") %>%
  rename(total_catch_num = lnr_num,
         total_catch_lbs = lnr_lbs) %>%
  # bind to tmp
  bind_rows(tmp) %>%
  # housing keeping tasks (trim columns)
  dplyr::select(fishery, group, total_catch_num, total_catch_lbs) %>%
  # join to all year and group combinations for full timeseries
  right_join(expand_grid(fishery = retained_catch$fishery,
                         group = unique(.$group)),
             by = c("fishery", "group")) %>%
  replace_na(list(total_catch_num = 0, total_catch_lbs = 0)) %>%
  # decipher fishery code
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # save output
  write_csv(here(paste0("tanner_crab/output/", season), "item4b_total_catch_legal_group.csv"))
  
    
  



# item 5 ----
## size composition from at-sea observers by size, sex, shell, and legal status in all BSAI fisheries

obs_meas %>%
  f_observer_size_comp(., by = 3, lump = T) %>%
  # save output
  write_csv(here(paste0("tanner_crab/output/", season), "item5_observer_size_comp.csv"))

# item 6 ----
## observer effort summary for directed tanner crab, snow crab and rkc fisheries

pot_sum %>%
  # count the number of unique observer pots
  group_by(fishery) %>%
  summarise(total_pots = n(),
            measure_pots = sum(msr_pot == "Y"),
            count_pots = sum(msr_pot == "N"),
            percent_measured = measure_pots / total_pots * 100) %>%
  # save output
  write_csv(here(paste0("tanner_crab/output/", season), "item6_observer_effort_summary.csv"))
  