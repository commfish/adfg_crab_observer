# notes ----
## fishery data request - tanner crab
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 3/10/2020

# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

# data inputs ----

## dockside data
dock <- read_csv(here("tanner_crab/data", "TANNER-1990-2019_dockside.csv"))

## observer crab detail data
obs_meas <- read_csv(here("tanner_crab/data", "TANNER-1990-2019_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("tanner_crab/data", "TANNER-1990-2019_potsum.csv"))

## fish ticket data
bd_ft_summary <- read_csv(here("./snow_crab/data", "retained_catch_all_fisheries.csv"))

# data management ----

## adjust historic fishery codes for data dumps
## overwrite object names
## add species code to pot_sum (all will be snow crab)
list(dock = dock, obs_meas = obs_meas, pot_sum = mutate(pot_sum, spcode = 932)) %>%
  purrr::map2(., c("dockside", "obs", "obs"), f_fish_code_adjust) %>%
  list2env(x = ., envir = .GlobalEnv)

## add fishery code to fish ticket data
bd_ft_summary %>%
  rename_all(tolower) %>%
  mutate(fishery = case_when(fishery == "snow" ~ paste0("QO", substring(year, 3, 4)),
                             fishery == "Tanner W" ~ paste0("QT", substring(year, 3, 4)),
                             fishery == "Tanner E" ~ paste0("TT", substring(year, 3, 4)),
                             fishery == "BBRKC" ~ paste0("TR", substring(year, 3, 4)))) -> bd_ft_summary


# item 1 ----
## directed effort in the tanner crab, snow crab and bbrkc fisheries

bd_ft_summary %>%
  # filter for only directed effort
  filter(dir_inc == "directed") %>%
  group_by(fishery) %>%
  summarise(potlifts = sum(fish_effort)) %>%
  # decode fishery code
  f_sdr(x = ., col = "fishery", type = "fishery_code") %>%
  # save output
  write_csv(here("tanner_crab/output", "item1_directed_effort.csv"))
  
  
# item 2 ----
# extract retained (directed and incidental) catch by fishery/season (in wide format)

bd_ft_summary %>%
  # get directed catch
  filter(grepl("TT|QT", fishery),
         dir_inc == "directed") %>%
  group_by(fishery) %>%
  summarise(directed_lb = sum(ret_cat_lb, na.rm = T),
            directed_t = directed_lb * 0.000453592,
            directed_crab = sum(ret_cat_crabs, na.rm = T)) %>%
  # join to incidenta catch
  left_join(bd_ft_summary %>%
              filter(grepl("TT|QT", fishery),
                     dir_inc == "incidental") %>%
              group_by(fishery) %>%
              summarise(incidental_lb = sum(ret_cat_lb, na.rm = T),
                        incidental_t = incidental_lb * 0.000453592,
                        incidental_crab = sum(ret_cat_crabs, na.rm = T)),
            by = "fishery") %>%
  # uncode fishery
  f_sdr(x = ., col = "fishery", type = "fishery_code") %>%
  # save output
  write_csv(here("tanner_crab/output", "item2_retained_catch.csv"))


# item 3 ----
# size composition of retained catch by shell condition, grouped as "new" and "old"
# directed fisheries only

dock %>%
  # compute size comp by lumped shell condition and fishery
  f_retained_size_comp(., lump = T) %>%
  # filter for directed tanner crab fisheries
  filter(grepl("QT|TT", fishery)) %>%
  ungroup() %>%
  # save output
  write_csv(here("tanner_crab/output", "item3_dockside_size_comp.csv"))

# item 4 ----

## prepare fraction of new shell in each fishery
obs_meas %>%
    # remove unknown shell conditions
    filter(shell >= 0) %>%
    # lump shell conditions into new and old
    mutate(shell = case_when(shell %in% c(0:2, 9) ~ 2,
                             shell %in% 3:5 ~ 3),
           group = case_when((sex == 2) ~ "female",
                             (sex == 1 & legal == 0) ~ "sublegal",
                             (sex == 1 & legal %in% c(1:3, 6)) ~ "tot_legal")) %>%
    filter(!is.na(group)) %>%
    # estimate fraction new shell
    group_by(fishery, group) %>%
    summarise(frac_new = sum(shell == 2) / n()) %>%
    ungroup() -> frac_new

## prepare avg wt for biomass expansion
obs_meas %>%
  # get average weights for biomass expansion
  f_average_wt(., by = 3, units = "lbs") %>%
  # add legal status text that matches pot sum groups
  mutate(group = case_when((sex == 2) ~ "female",
                           (sex == 1 & legal_status == F) ~ "sublegal",
                           (sex == 1 & legal_status == T) ~ "tot_legal")) %>%
  ungroup() %>%
  dplyr::select(fishery, group, shell_lump, avg_wt) -> avg_wt_lbs

## get total directed effort in BBRKC, BSTC, and BSSC fisheries
bd_ft_summary %>%
  # filter for only directed effort
  filter(dir_inc == "directed") %>%
  group_by(fishery) %>%
  summarise(effort = sum(fish_effort)) -> total_effort

## summarise count pot data and expand
pot_sum %>%
  # filter for only BBRKC, BSTC, and BSSC fisheries
  filter(grepl("QT|TT|TR", fishery)) %>%
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
  # join to fraction compute new shell and old and new shell estimates
  left_join(frac_new, by = c("fishery", "group")) %>%
  mutate(`2` = tot_catch_number * frac_new,
         `3` = tot_catch_number * (1 - frac_new)) %>%
  # remove total catch of both shell conditions (conflicting name)
  dplyr::select(-tot_catch_number) %>%
  # pivot to long format
  pivot_longer(c(`2`, `3`), names_to = "shell_lump", values_to = "tot_catch_number") %>%
  mutate(shell_lump = as.numeric(shell_lump)) %>%
  # join to avg_wt_lbs
  left_join(avg_wt_lbs, by = c("fishery", "group", "shell_lump")) %>%
  # use timeseries average weight if subgroup was caught in count pots, but not measure pots
  mutate(area = substring(fishery, 1, 2)) %>%
  group_by(substring(fishery, 1, 2), group) %>%
  mutate(ts_avg_wt = mean(avg_wt, na.rm = T)) %>%
  ungroup() %>%
  mutate(avg_wt = ifelse(count > 0 & is.na(avg_wt), ts_avg_wt, avg_wt)) %>%
  # compute total catch in weight
  mutate(tot_catch_lb = tot_catch_number * avg_wt,
         tot_catch_t = tot_catch_lb * 0.000453592) %>%
  # decode fishery and shell condition
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  f_sdr(., col = "shell_lump", type = "shell_condition") %>%
  # reorder and save
  dplyr::select(1:5, 12:13, 17:18) %>%
  # save output
  write_csv(., here("tanner_crab/output", "item4_total_catch.csv"))

# item 5 ----
## size composition from at-sea observers by size, sex, shell, and legal status in all BSAI fisheries

obs_meas %>%
  filter(grepl("QT|TT|TR", fishery)) %>%
  f_observer_size_comp(., by = 3, lump = T) %>%
  # add a column for total
  mutate(total = new + old) %>%
  # save output
  write_csv(here("tanner_crab/output", "item5_observer_size_comp.csv"))

# item 6 ----

## observer effort summary for directed tanner crab, snow crab and rkc fisheries
pot_sum %>%
  filter(grepl("QT|TT|TR", fishery)) %>%
  # count the number of unique observer pots
  group_by(fishery) %>%
  summarise(total_pots = n(),
            measure_pots = sum(msr_pot == "Y"),
            count_pots = sum(msr_pot == "N"),
            percent_measured = measure_pots / total_pots * 100) %>%
  # save output
  write_csv(here("tanner_crab/output", "item6_observer_effort_summary.csv"))
  





# item 4a old code (saved in case we go back to expanding from measure pots) ----
# # total catch estimation for each fishery by sex and shell condition
# 
# ## get total observer effort by fishery
# pot_sum %>%
#   # filter for measure pots only
#   filter(msr_pot == "Y") %>%
#   group_by(fishery) %>%
#   summarise(meas_effort = n()) -> meas_effort
# 
# ## get total fishery effort by fishery
# bd_ft_summary %>%
#   # filter for only directed effort
#   filter(dir_inc == "directed") %>%
#   dplyr::select(fishery, fish_effort) %>%
#   rename(effort = fish_effort)  -> directed_effort
# 
# ## estimate total catch
# obs_meas %>%
#   # get number of crab by fishery, size, sex, shell and maturity (for calc weight)
#   dplyr::select(fishery, sex, shell) %>%
#   # lump shell condition into "new" (2), "old" (3), and unknown (-9)
#   mutate(shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
#                                 shell %in% c(3:5) ~ 3,
#                                 is.na(shell) ~ -9,
#                                 shell == -9 ~ -9)) %>%
#   count(fishery, sex, shell_lump) %>%
#   rename(count = n) %>%
#   # join with observer measure pot effort by fishery
#   left_join(meas_effort, by = "fishery") %>%
#   # join with total directed effort by fishery
#   left_join(directed_effort, by = "fishery") %>%
#   # comput total catch by line, in numbers of crab
#   mutate(total_catch_num = (count / meas_effort) * effort) %>%
#   # join to average weight per group
#   left_join(bind_rows(f_average_wt(x = obs_meas, by = 2, units = "lbs"),
#                       f_average_wt(x = obs_meas, by = 1, units = "lbs") %>%
#                         mutate(shell_lump = -9)),
#             by = c("fishery", "sex", "shell_lump")) %>%
#   # scale total_catch_num to total_catch_lbs
#   mutate(total_catch_lbs = total_catch_num * avg_wt) %>% 
#   # dicepher sex and shell condition codes
#   f_sdr(col = "sex", type = "sex") %>%
#   f_sdr(col = "shell_lump", type = "shell_condition") %>%
#   # housing keeping task (trim columns, fill NA, and rename)
#   dplyr::select(fishery, sex_text, shell_text, total_catch_num, total_catch_lbs) %>%
#   replace_na(list(shell_text = "unknown")) %>%
#   rename(shell = shell_text,
#          sex = sex_text) %>%
#   # join to all year and group combinations for full timeseries
#   right_join(expand_grid(fishery = directed_effort$fishery,
#                          sex = unique(.$sex),
#                          shell = unique(.$shell)),
#              by = c("fishery", "sex", "shell")) %>%
#   replace_na(list(total_catch_num = 0, total_catch_lbs = 0)) %>%
#   # decipher fishery code 
#   f_sdr(col = "fishery", type = "fishery_code")
#   # save output
#   write_csv(here(paste0("tanner_crab/output/", season), "item4a_total_catch_sex_shell.csv"))



