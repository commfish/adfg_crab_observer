# notes ----
## fishery data request - snow crab
## prepared by: Tyler Jackson
## email: tyler.jackson@alaska.gov
## last updated: 6/22/2020


# load ----
## packages
library(tidyverse)

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


## fishery metadata
## use read.csv instead of read_csv to avoid misreading string...something wrong with spacing 
## in fishery_name
fishery_meta <- as_tibble(read.csv("./misc/data/adfg_fishery_metadata.csv", stringsAsFactors = F))

## calculated weight parameters
read_csv("./misc/data/calc_weight_parameters.csv") %>%
  filter(species == "snow crab") -> params

# item 1 ----
## size frequency of retained catch by shell in directed snow crab fishery


dock %>%
  # filter for the most recent directed fishery
  filter(fishery == paste0("QO", substring(season, 3, 4))) %>%
  dplyr::select(size, shell, numcrab) %>%
  # sum all samples by size and shell condition
  group_by(size, shell) %>%
  summarise_at(.vars = "numcrab", sum) %>%
  # change shell condition names to text
  mutate(shell = case_when(shell == 0 ~ "molting",
                           shell == 1 ~ "soft", 
                           shell == 9 ~ "new_pliable",
                           shell == 2 ~ "new",
                           shell == 3 ~ "old",
                           shell == 4 ~ "very_old",
                           shell == 5 ~ "very_very_old")) %>%
  # pivot to wide format
  pivot_wider(names_from = shell, values_from = numcrab) %>%
  # replace NA with 0
  replace(is.na(.), 0) %>%
  # add total column
  bind_cols(tibble(total = rowSums(.))) %>%
  # save output
  write_csv("./snow_crab/output/2019_20/item1_dockside_size_comp.csv")
  
         
            
# item 2 ----
## size frequency of snow crab bycatch in other BSAI crab fisheries


obs_meas %>%
  # filter for non-directed fisheries in most recent season
  filter(fishery %in% grep(substring(season, 3, 4), fishery, value = T),
         substring(fishery, 1, 2) != "QO") %>%
  dplyr::select(fishery, sex, size, shell) %>%
  # get a count per group
  count(fishery, sex, size, shell) %>%
  rename(count = n) %>%
  # add fishery text, legal text, and shell text 
  mutate(legal = case_when((sex == 1 & size >= 78) ~ TRUE, 
                           (sex == 1 & size < 78) ~ FALSE)) %>%
  left_join(fishery_meta, c("fishery" = "fishery_code")) %>%
  mutate(shell_text = case_when(shell == 0 ~ "molting",
                                shell == 1 ~ "soft", 
                                shell == 9 ~ "new_pliable",
                                shell == 2 ~ "new",
                                shell == 3 ~ "old",
                                shell == 4 ~ "very_old",
                                shell == 5 ~ "very_very_old")) %>%
  dplyr::select(1, 8, 2:3, 6, 4, 11, 5) %>%
  # save output
  write_csv("./snow_crab/output/2019_20/item2_snow_crab_bycatch_size_comp.csv")
         
# item 3 ----
## retained catch in numbers and biomass (total for directed fishery)


fish_tick %>%
  # filter fr directed snow crab fishery
  filter(fishery %in% grep("QO", fishery, value = T)) %>%
  # sum retained catch across stat areas
  group_by(fishery) %>%
  summarise_at(4:8, sum, na.rm = T) %>%
  # add fishery description
  left_join(fishery_meta, c("fishery" = "fishery_code")) %>%
  dplyr::select(1, 8, 2:6) %>%
  # save output
  write_csv("./snow_crab/output/2019_20/item3_snow_crab_retained_catch.csv")



# item 4 ----
## total catch of male and female snow crab in BSAI crab fisheries


## get the average weight of a snow crab by sex and fishery
obs_meas %>%
  # filter for the most recent fisheries in which snow crab were caught
  filter(substring(fishery, 3, 4) %in% substring(season, 3, 4)) %>%
  dplyr::select(fishery, sex, size, shell, clutch) %>%
  count(fishery, sex, size, shell, clutch) %>%
  rename(count = n) %>%
  # add text for sex and maturity
  mutate(sex = case_when(sex == 1 ~ "male",
                         sex == 2 ~ "female"),
         maturity = ifelse(sex == "male", NA, 
                    ifelse((sex == "female" & clutch > 0) | (sex == "female" & is.na(clutch)), 
                           "mature", "immature"))) %>%
  # join to growth parameters
  left_join(params, by = c("sex", "maturity")) %>%
  # compute calculated weight 
  mutate(wt_kg = (alpha * as.numeric(size)^beta) / 1000) %>%
  # get average weight for a male and a female
  group_by(fishery, sex) %>%
  summarise(total_count = sum(count), 
            avg_wt_kg = weighted.mean(wt_kg, w = count)) -> avg_wt_kg 

## summarise count pot data
pot_sum %>%
  # filter for the most recent fisheries in which snow crab were caught
  filter(substring(fishery, 3, 4) %in% substring(season, 3, 4)) %>%
  dplyr::select(-tot_legal) %>%
  # count the number of unique observer pots
  group_by(fishery) %>%
  mutate(total_pots = n()) %>%
  pivot_longer(9:13, names_to = "group", values_to = "count") %>%
  group_by(fishery, group) %>%
  summarise(total_pots = mean(total_pots),
            count = sum(count, na.rm = T)) %>%
  # compute cpue
  mutate(cpue = count / total_pots) %>%
  # scale to total effort
  left_join(fish_tick %>%
              group_by(fishery) %>%
              summarise(effort = sum(effort, na.rm = T)), by = "fishery") %>%
  mutate(tot_catch_number = cpue * effort) %>%
  # add total catch in weight
  mutate(sex = ifelse(group == "female", "female", "male")) %>%
  left_join(avg_wt_kg %>%
              dplyr::select(fishery, sex, avg_wt_kg), by = c("fishery", "sex")) %>%
  # compute total catch in weight kg
  mutate(tot_catch_wt_kg = tot_catch_number * avg_wt_kg) %>%
  # sum catch by fishery and sex
  group_by(fishery, sex) %>%
  summarise(tot_catch_number = sum(tot_catch_number),
            tot_catch_wt_lbs = sum(tot_catch_wt_kg) * 2.2046226218) %>%
  # replace missing value with 0
  replace_na(list(tot_catch_number = 0,
                  tot_catch_wt_lbs = 0)) %>%
  write_csv(., "./snow_crab/output/2019_20/item_4_total_bycatch.csv")




# item 5 ----
## retained catch in numbers and biomass (by stat area for directed fishery)


fish_tick %>%
  # filter fr directed snow crab fishery
  filter(fishery %in% grep("QO", fishery, value = T)) %>%
  # add fishery description
  left_join(fishery_meta, c("fishery" = "fishery_code")) %>%
  # remove cpue, avg_wt, and price fields
  dplyr::select(1, 14, 2:9) %>%
  # save output
  write_csv("./snow_crab/output/2019_20/item5_snow_crab_fish_ticket_stat_area.csv")


