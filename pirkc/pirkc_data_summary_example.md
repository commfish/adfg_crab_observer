Pribilof Islands Red King Crab Data Summary Example
================

## Data Summary

The following document details an example workflow to produce the
following outputs:

  - **item1\_retained\_catch\_size\_comp.csv** - Size frequency of
    retained catch shell condition in the directed fishery.
  - **item2\_observer\_catch\_size\_comp.csv** - Size frequency of crab
    caught in observer pots by shell condition during Bering Sea
    fisheries.
  - **item3\_total\_retained\_catch.csv** - Total retained catch in the
    previous season’s directed fishery.
  - **item4\_total\_catch\_by\_fishery.csv** - Point estimates of total
    catch of females, sublegal males, and legal males in the previous
    season’s Bering Sea fisheries.
  - **item5\_fish\_ticket\_summary\_stat\_area.csv** - Raw fish ticket
    summary by stat area for the previous season’s directed fishery.

Since the PIRKC fishery has not been open in recent history, items 1, 3,
and 5 may produce empty .csv files.

## Example Workflow

### Load libraries, global options, custom functions, and raw data

``` r
# load ----
## custom functions and libraries
source("../misc/code/custom_functions.R")

## most recent season
season <- "2019_20"

# data inputs ----

## dockside data
dock <- read_csv(here("pirkc/data", "RKC-1990-2019_dockside.csv"))

## observer crab detail data
### rkc in king and tanner crab fisheries
obs_meas <- read_csv(here("pirkc/data", "RKC-1990-2019_crab_dump.csv"))
### rkc in snow crab fisheries 
obs_meas_QO <- do.call("rbind", lapply(list.files("../pirkc/data/crab_dump_QO", full.names = T), read_csv))

## count pot data
### rkc in king and tanner crab fisheries
pot_sum <- read_csv(here("pirkc/data", "RKC-1990-2019_potsum.csv"))
### rkc in snow crab fisheries 
pot_sum_QO <- do.call("rbind", lapply(list.files("../pirkc/data/potsum_QO", full.names = T), read_csv))

## fish ticket data by stat area
fish_tick <- read_csv(here("pirkc/data", "bsai_crab_fish_ticket_summary_stat_area.csv"))
```

Because observer measure pot and pot summary data do not include the
directed snow crab fishery (loaded separately above), the separate data
inputs must be combined. Columns for pot summary data must be corrected.

``` r
# data management ----

## trim columns and join files for measure pot data
obs_meas %>%
  bind_rows(dplyr::select(obs_meas_QO, names(.))) -> obs_meas


## combine pot summary data
## fix column names in snow crab fishery pot summary
names(pot_sum_QO) <- c("fishery", "trip", "adfg", "sampdate", "spn", "statarea", "latitude","longitude", 
                       "e_W", "depth", "soak", "gear", "biotwine", "ring_size", "mesh", "female",
                       "sublegal", "legal_ret", "legal_nr", "legal_ur", "tot_legal", "msr_pot")
## bind rows
pot_sum %>%
  dplyr::select(-start_year) %>%
  bind_rows(dplyr::select(pot_sum_QO, names(.)[-2])) -> pot_sum
```

### Item 1

Sum the counts of landed crab (as collected by dockside samplers) by
shell condition in 1 mm size bins for the previous season’s directed
PIRKC crab fishery.

``` r
dock %>%
  f_retained_size_comp(., lump = F) %>%
  # add row sums
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 2) == "QR",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item1_retained_catch_size_comp.csv")))
```

### Item 2

Sum counts of crab (as measured by at-sea observers) by sex and shell
condition in 1 mm size bins for each the most recent Bering Sea
fisheries.

``` r
obs_meas %>%
  f_observer_size_comp(., by = 2, lump = F) %>%
  # add row sums
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 1) == "Q",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item2_observer_catch_size_comp.csv")))
```

### Item 3

Sum the raw fish ticket report for the most recent directed fishery
across all stat areas to get totals for number and weight (lbs) of live
retained crab and deadloss, as well as total effort. Note: *total number
of crab at a delivery is expanded from brailer subsample weights and
counts*.

``` r
fish_tick %>%
  # sum across stat areas
  group_by(fishery) %>%
  dplyr::select(-stat_area) %>%
  summarise_all(sum, na.rm = T) %>%
  # sum live crab and deadloss
  transmute(fishery = fishery,
            retained_number = live_number + deadloss_number,
            retained_lbs = live_lbs + deadloss_lbs, 
            effort = effort) %>%
  # decipher fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 2) == "QR",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item3_total_retained_catch.csv")))
```

### Item 4

Total catch estimates of subgroups (sex and legal status group: female,
sublegal, total legal) are expanded using counts of each subgroup
recorded in observer *count* and *measure* pots. Counts are standardized
to CPUE using total observed pots in each Bering Sea fishery, and
expanded to total catch by multiplying by the total fishery effort as
provided via fish ticket summaries. Total catch weight (lbs) is
estimated by multiplying total catch number by the average weight of a
crab for each subgroup in that fishery, which is estimated from
*measure* pot data. Note: *total catch estimates are provided here as
point estimates without associated variance, for now*.

``` r
## get fishery directed effort
fish_tick %>%
  group_by(fishery) %>%
  summarise(directed_effort = sum(effort, na.rm = T)) -> directed_effort

pot_sum %>%
  # sum females, sublegal and legal males by fishery, include observer effort
  group_by(fishery) %>%
  summarise(female = sum(female, na.rm = T),
            sublegal = sum(sublegal, na.rm = T),
            tot_legal = sum(tot_legal, na.rm = T),
            obs_effort = n()) %>%
  # pivot to long format
  pivot_longer(c(female, sublegal, tot_legal), names_to = "group", values_to = "count") %>%
  # join to directed effort
  left_join(directed_effort, by = "fishery") %>%
  # join to average weight by sex
  mutate(sex = ifelse(group == "female", 2, 1)) %>%
  left_join(f_average_wt(x = obs_meas, by = 1, units = "lbs"),
            by = c("fishery", "sex")) %>%
  # compute total catch number and weight
  mutate(total_catch_number = (count / obs_effort) * directed_effort,
         total_catch_lbs = total_catch_number * avg_wt) %>%
  # trim columns, filter for most recent season's fishery in Bering sea
  dplyr::select(fishery, group, total_catch_number, total_catch_lbs) %>%
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(substring(fishery, 1, 1) == "Q", 
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item4_total_catch_by_fishery.csv")))
```

### Item 5

Save the raw fish ticket summary by stat area, subset for the directed
PIRKC fshery in the most recent season.

``` r
fish_tick %>%
  # trim columns
  dplyr::select(-cpue, -avg_wt, -price_lbs) %>%
  # decipher fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # filter for pirkc fisheries in most recent season
  filter(substring(fishery, 1, 2) == "QR",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("pirkc/output/", season, "/item5_fish_ticket_summary_stat_area.csv")))
```
