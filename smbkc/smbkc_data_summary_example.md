St Matthew Island Blue King Crab Data Summary Example
================

## Data Summary

The following document details an example workflow to produce the
following output:

  - **item1\_total\_catch\_by\_fishery.csv**- Point estimates of total
    catch (number of crab and pounds) for female, sublegal, and total
    legal crab in the most recent season, by fishery.
  - **item2\_directed\_fishery\_stats.csv**- Total retained catch in the
    previous season’s directed fishery.
  - **item3a\_sublegal\_observer\_size\_comp\_smbkc\_fishery.csv**-
    Carapace length (mm) composition of sublegal males by shell
    condition caught in observer measure pots in the Bering Sea snow
    crab fishery.
  - **item3b\_legal\_observer\_size\_comp\_smbkc\_fishery.csv**-
    Carapace length (mm) composition of legal males by shell condition
    caught in observer measure pots in the Bering Sea snow crab fishery.
  - **item3c\_female\_observer\_size\_comp\_smbkc\_fishery.csv**-
    Carapace length (mm) composition of females by shell condition
    caught in observer measure pots in the Bering Sea snow crab fishery.
  - **item4\_retained\_size\_comp.csv**- Carapace length (mm)
    composition of retained catch by shell condition as recorded by
    dockside samplers in the previous season’s SMBKC directed fishery
    (if open).
  - **item5a\_sublegal\_observer\_size\_comp\_snow\_crab\_fishery.csv**-
    Carapace length (mm) composition of sublegal males by shell
    condition caught in observer measure pots in the Bering Sea snow
    crab fishery.
  - **item5b\_legal\_observer\_size\_comp\_snow\_crab\_fishery.csv**-
    Carapace length (mm) composition of legal males by shell condition
    caught in observer measure pots in the Bering Sea snow crab fishery.
  - **item5c\_female\_observer\_size\_comp\_snow\_crab\_fishery.csv**-
    Carapace length (mm) composition of females by shell condition
    caught in observer measure pots in the Bering Sea snow crab fishery.

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
dock <- read_csv(here("smbkc/data", "BKC-1990-2019_dockside.csv"))

## observer crab detail data
### rkc in king and tanner crab fisheries
obs_meas <- read_csv(here("smbkc/data", "BKC-1990-2019_crab_dump.csv"))

## count pot data
### rkc in king and tanner crab fisheries
pot_sum <- read_csv(here("smbkc/data", "BKC-1990-2019_potsum.csv"))
### remove column start_year(not in standard obs pot summary data format)
pot_sum <- dplyr::select(pot_sum, -start_year)

## fish ticket data by stat area
fish_tick <- read_csv(here("smbkc/data", "bsai_crab_fish_ticket_summary_stat_area.csv"))
```

### Item 1

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
  # trim columns, filter for most recent season's fisheries
  dplyr::select(fishery, group, total_catch_number, total_catch_lbs) %>%
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season, "/item1_total_catch_by_fishery.csv")))
```

### Item 2

Sum the raw fish ticket report for the previous season’s directed
fishery across all stat areas to get totals for number and weight (lbs)
of live retained crab and deadloss, as well as total effort. Note:
*total number of crab at a delivery is expanded from brailer subsample
weights and counts*.

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
  filter(substring(fishery, 1, 2) == "QP",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season, "/item2_directed_fishery_stats.csv")))
```

### Item 3

Summed counts of blue king crab caught in observer measure pots during
the previous season’s directed SMBKC fishery by shell condition in 1 mm
size bins for a) sublegal males, b) legal males, and c) females. Each
group is output as a separate .csv file.

``` r
## sublegal males by shell condition
obs_meas %>%
  filter(sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item3a_sublegal_observer_size_comp_smbkc_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item3b_legal_observer_size_comp_smbkc_fishery.csv"))

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item3c_female_observer_size_comp_smbkc_fishery.csv"))
```

### Item 4

Sum the counts of landed crab (as collected by dockside samplers) by
shell condition in 1 mm size bins for the previous season’s directed
PIRKC crab fishery.

``` r
dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QP") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item4_retained_size_comp.csv"))
```

### Item 5

Summed counts of blue king crab caught in observer measure pots during
the previous season’s Bering Sea snow crab fishery by shell condition in
1 mm size bins for a) sublegal males, b) legal males, and c) females.
Each group is output as a separate .csv file.

``` r
## sublegal males by shell condition
obs_meas %>%
  filter(sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QO") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item5a_sublegal_observer_size_comp_snow_crab_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QO") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item5b_legal_observer_size_comp_snow_crab_fishery.csv"))

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for most recent directed fishery
  filter(opening_year == as.numeric(substring(season, 1, 4)),
         substring(fishery, 1, 2) == "QO") %>%
  # save output
  write_csv(here(paste0("smbkc/output/", season), "item5c_female_observer_size_comp_snow_crab_fishery.csv"))
```
