# notes ----
## fishery data request - snow crab
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 6/22/2020


# load ----
## custom functions and libraries
source("./misc/code/custom_functions.R")

## most recent season
season <- "2019_20"

# data inputs ----

## dockside data
dock <- read_csv("./snow_crab/data/SNOWCRAB-1990-2019_dockside.csv")

## observer crab detail data
obs_meas <- read_csv("./snow_crab/data/SNOWCRAB-1990-2019_crab_dump.csv")

## count pot data
pot_sum <- read_csv("./snow_crab/data/SNOWCRAB-1990-2019_potsum.csv")

## stat area fish ticket summary for all fisheries directed catch
fish_tick <- read_csv("./snow_crab/data/bsai_crab_fish_ticket_summary_stat_area.csv")


# item 1 ----
## size frequency of retained catch by shell condition in directed snow crab fishery
## shell codes are lumped to broad catagories of "new" and "old"

dock %>%
  # uncode fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # filter for most current year
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         target == "snow_crab") %>%
  # generate size composition
  f_retained_size_comp(x = ., lump = T) %>%
  # add a column for total
  mutate(total = rowSums(.[, which(names(.) %in% shell_levels)])) %>%
  # save output
  write_csv(here(paste0("snow_crab/output/", season), "item1_dockside_size_comp.csv"))
  
         
            
# item 2 ----
## size frequency of snow crab catch in all BSAI crab fisheries

obs_meas %>%
  # combine BBRKC directed and test fishery data
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # uncode fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # filter for most current year
  filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
  # generate size composition
  f_observer_size_comp(x = ., by = 3, lump = T) %>%
  # add a column for total (using generalized code to account for any possible shell condition)
  mutate(total = rowSums(.[, which(names(.) %in% shell_levels)])) %>%
  # save output
  write_csv(here(paste0("snow_crab/output/", season), "item2_snow_crab_observer_size_comp.csv"))
  
  
  
  
         


# item 3 ----
## retained catch in numbers and biomass (total for directed fishery)


fish_tick %>%
  # sum retained catch across stat areas
  group_by(fishery) %>%
  summarise_at(4:8, sum, na.rm = T) %>%
  # add fishery description, filter for directed snow crab fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  filter(target == "snow_crab",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("snow_crab/output/", season), "item3_snow_crab_retained_catch.csv"))



# item 4 ----
## total catch of male and female snow crab in BSAI crab fisheries

### prepare avg wt for biomass expansion
obs_meas %>%
  # combine BBRKC directed and test fishery data
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # uncode fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # filter for most current year
  filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
  # get average weights for biomass expansion
  f_average_wt(., by = 4, units = "lbs") %>%
  # add legal status text that matches pot sum groups
  mutate(group = case_when((sex == 2) ~ "female",
                           (sex == 1 & legal_status == F) ~ "sublegal",
                           (sex == 1 & legal_status == T) ~ "tot_legal")) %>%
  ungroup() %>%
  dplyr::select(fishery, group, avg_wt) -> avg_wt_lbs

### prepare total effort
fish_tick %>%
  # combine BBRKC directed and test fishery data
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # aggregate effort by fishery
  group_by(fishery) %>%
  summarise(effort = sum(effort, na.rm = T)) -> total_effort

### summarise count pot data and expand
pot_sum %>%
  # combine BBRKC directed and test fishery data
  mutate(fishery = gsub("XR", "TR", fishery)) %>%
  # uncode fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  # filter for most current year
  filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
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
  # join to avg_wt_lbs and compute total catch in weight
  left_join(avg_wt_lbs, by = c("fishery", "group")) %>%
  mutate(tot_catch_wt = tot_catch_number * avg_wt) %>%
  # replace missing value with 0
  replace_na(list(tot_catch_number = 0,
                  tot_catch_wt = 0)) %>%
  # save output
  write_csv(., here(paste0("snow_crab/output/", season), "item4_total_catch.csv"))




# item 5 ----
## retained catch in numbers and biomass (by stat area for directed fishery)


fish_tick %>%
  # add fishery description, filter for sow crab directed fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  filter(target == "snow_crab",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # remove cpue, avg_wt, and price fields
  dplyr::select(-cpue, -avg_wt, -price_lbs) %>%
  # save output
  write_csv(here(paste0("snow_crab/output/", season), "item5_snow_crab_fish_ticket_stat_area.csv"))


