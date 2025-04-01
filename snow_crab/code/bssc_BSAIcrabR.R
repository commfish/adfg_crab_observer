# notes ----

## Example BSSC workflow using BSAIcrabR
## tyler jackson
## 3/5/2025

# load ----

#devtools::install_github("commfish/BSAIcrabR", force = T)
library(BSAIcrabR)

# data ----

## count pot data
pot_sum <- load_pot_dump("./snow_crab/data/SNOWCRAB-1990-2024_potsum.csv", stock = "BSSC", clean = T)

## measure pot data
obs_meas <- load_crab_dump("./snow_crab/data/SNOWCRAB-1990-2024_crab_dump.csv", stock = "BSSC", clean = T)

## dockside data
dock <- load_dockside("./snow_crab/data/SNOWCRAB-1990-2024_retained_size_freq.csv", stock = "BSSC", clean = T)

## timeseries of directed effort
dir_effort_prerat <- read_csv("./misc/data/fish_ticket_timeseries/directed_effort_timeseries_DP.csv") 

## fish ticket summary by stat area
readRDS("../adfg_crab_observer/misc/data/fish_ticket_timeseries/fish_ticket_stat_area_summary_2_26_25.RDS") %>%
  # filter for snow crab directed fisheries before 2005
  filter(grepl("QO", fishery),
         crab_year < 2005) %>%
  # rename fields
  rename(tot_live_n = live_number,
         tot_deadloss_n = deadloss_number,
         tot_live_lb = live_lbs,
         tot_deadloss_lb = deadloss_lbs) %>%
  bind_rows(read_csv("../adfg_crab_observer/misc/data/fish_ticket_timeseries/Q_dir_inc_fish_ticket.csv", skip = 2) %>%
              janitor::clean_names() %>%
              filter(grepl("Total", seasons), !grepl("Grand", seasons)) %>%
              transmute(crab_year = as.numeric(substring(seasons, 1, 4)),
                        fishery = ifelse(crab_year == 2005, "QO05r", paste0("QO", substring(crab_year, 3, 4))),
                        tot_live_n = bss_total_crab,
                        tot_live_lb = bss_total_pounds,
                        dir_live_n = bss_directed_crab,
                        dir_deadloss_n = NA,
                        dir_live_lb = bss_directed_pounds,
                        dir_deadloss_lb = NA, 
                        inc_live_n = bss_incidental_crab,
                        inc_deadloss_n = NA,
                        inc_live_lb = bss_incidental_pounds,
                        inc_deadloss_lb = NA)) -> ft_dir_inc
  

## incidental-directed fish ticket report
read_csv("../adfg_crab_observer/misc/data/fish_ticket_timeseries/Q_dir_inc_fish_ticket.csv", skip = 2) %>%
  janitor::clean_names() %>%
  filter(grepl("Total", seasons)) %>%
  filter(!grepl("Grand", seasons)) %>%
  mutate(year = substring(seasons, 3, 4)) %>%
  transmute(year, QT = wbt_directed_effort, QO = bss_directed_effort) %>%
  left_join(read_csv("../adfg_crab_observer/misc/data/fish_ticket_timeseries/T_dir_inc_fish_ticket.csv", skip = 2) %>%
              janitor::clean_names() %>%
              filter(grepl("Total", seasons)) %>%
              filter(!grepl("Grand", seasons)) %>%
              mutate(year = substring(seasons, 3, 4)) %>%
              transmute(year, TT = ebt_directed_effort, TR = bbr_directed_effort) %>%
              full_join(readRDS("../adfg_crab_observer/misc/data/fish_ticket_timeseries/fish_ticket_stat_area_summary_2_26_25.RDS") %>%
                           filter(substring(fishery, 1, 2) == "XR") %>%
                           mutate(year = substring(fishery, 3, 4)) %>%
                           group_by(year) %>%
                           summarise(XR = sum(pots, na.rm = T))), by = "year") %>%
  pivot_longer(2:ncol(.), names_to = "fish", values_to = "effort") %>%
  # correct bbrkc test fishery
  mutate(fish = gsub("XR", "TR", fish)) %>%
  mutate(fishery = paste0(fish, year)) %>%
  # add crab year
  add_crab_year(date_correct = F) %>%
  group_by(fishery, crab_year) %>%
  summarise(effort = sum(effort, na.rm = T)) %>% 
  # fix code for QO05r
  mutate(fishery = ifelse(fishery == "QO05", "QO05r", fishery)) %>%
  # join to prerationalized doug pengilly data
  bind_rows(dir_effort_prerat) %>%
  # other fisheries
  bind_rows(readRDS("../adfg_crab_observer/misc/data/fish_ticket_timeseries/fish_ticket_stat_area_summary_2_26_25.RDS") %>%
              filter(!(substring(fishery, 1, 2) %in% c("QO", "TT", "TR", "QT", "XR"))) %>%
              filter(!(fishery %in% c("2005 before rationalized", "Sheet1"))) %>%
              group_by(fishery, crab_year) %>%
              summarise(effort = sum(pots, na.rm = T))) %>% ungroup %>%
  arrange(fishery) %>%
  # temporarily add CP98 (as QP)
  add_row(fishery = "QP98", effort = 89500, crab_year = 1998) %>%
  group_by(fishery, crab_year) %>%
  summarise(effort = sum(effort)) %>% ungroup -> dir_effort

  
# retained catch ----

get_retained_catch(ft_data = ft_dir_inc, stock = "BSSC") %>%
  add_target_stock() %>%
  write_csv("./snow_crab/output/2025/retained_catch.csv")

# total catch ----

get_total_catch(pot_data = pot_sum, crab_data = obs_meas, ft_data = dir_effort, stock = "BSSC") %>% 
  add_target_stock() %>%
  write_csv("./snow_crab/output/2025/total_catch.csv") 

get_discards(retained_catch = get_retained_catch(ft_data = ft_dir_inc, stock = "BSSC"),
             total_catch = get_total_catch(pot_data = pot_sum, crab_data = obs_meas, ft_data = dir_effort, stock = "BSSC"), 
             stock = "BSSC") %>% add_target_stock() %>% 
  write_csv("./snow_crab/output/2025/bssc_discards.csv")

# retained size comp ----

get_dockside_comp(data = dock, by = "shell", lump = T) %>% 
  add_target_stock() %>% 
  write_csv("./snow_crab/output/2025/retained_catch_composition.csv")

# observer size comp ----

obs_comp <- get_observer_comp(data = obs_meas, by = c("sex", "shell"), lump = T)

## directed fishery
obs_comp %>% filter(substring(fishery, 1, 2) == "QO") %>%
  add_target_stock() %>% 
  write_csv("./snow_crab/output/2025/directed_total_composition.csv")

## all other crab fisheries
obs_comp %>% filter(substring(fishery, 1, 2) != "QO") %>%
  add_target_stock() %>% 
  write_csv("./snow_crab/output/2025/crab_bycatch_composition.csv")


