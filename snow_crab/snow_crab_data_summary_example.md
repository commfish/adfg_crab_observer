Snow Crab Data Summary Example
================

## Data Summary

The following document details an example workflow to produce the
following output documents:

  - **item1\_dockside\_size\_comp.csv** - Size frequency by shell
    condition of retained catch. Shell conditions lumped into “new” and
    “old” catagories.
  - **item2\_snow\_crab\_bycatch\_size\_comp.csv** - Size frequency of
    snow crab caught in every BSAI crab fishery (including the directed
    snow crab fishery) by sex, shell condition, legal status (i.e., T/F;
    True = male, CW ≥ 78 mm), and fishery. Shell conditions lumped into
    “new” and “old” catagories.
  - **item3\_snow\_crab\_retained\_catch.csv** - Total retained live
    catch and deadloss (from fish ticket data) for the directed snow
    crab fishery.
  - **item4\_total\_catch.csv** - Point estimates of total catch number
    and total catch weight (lbs) of snow crab by sex and legal status
    group (e.g., female, sublegal, and total legal) in each crab fishery
    in which snow crab were caught.
  - **item5\_snow\_crab\_fish\_ticket\_stat\_area.csv** - Fish ticket
    data for the directed snow crab fishery summarised by statstical
    area.

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
dock <- read_csv(here("snow_crab/data", "SNOWCRAB-1990-2019_dockside.csv"))

## observer crab detail data
obs_meas <- read_csv(here("snow_crab/data", "SNOWCRAB-1990-2019_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("snow_crab/data", "SNOWCRAB-1990-2019_potsum.csv"))

## statistical area fish ticket summary for all fisheries landed catch
fish_tick <- read_csv(here("snow_crab/data", "bsai_crab_fish_ticket_summary_stat_area.csv"))
```

### Item 1

Sum the counts of landed crab (as collected by dockside samplers) by
shell condition in 1 mm size bins for the most recent directed snow crab
fishery. Shell conditions are lumped into to broader catagories “new”
(includes molting, soft, pliable, new) and “old” (includes old, very
old, very very old).

``` r
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
```

### Item 2

Sum counts of crab (as measured by at-sea observers) by sex, shell
condition (lumped into “new” and “old”) and legal status (T/F) in 1 mm
size bins for each the most recent BSAI fisheries. Size compositions are
produced separately for each fishery, except for BBRKC directed and test
fisheries, which are combined. Only snow caught in *measured* pots are
included.

``` r
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
```

### Item 3

Sum the raw fish ticket report for the most recent directed fishery
across all stat areas to get totals for number and weight (lbs) of live
retained crab and deadloss, as well as total effort. Note: *total number
of crab at a delivery is expanded from brailer subsample weights and
counts*.

``` r
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
```

### Item 4

Total catch estimates of snow crab subgroups (sex and legal status
group: female, sublegal, total legal) are expanded using counts of each
subgroup recorded in observer *count* and *measure* pots. Counts are
standardized to CPUE using total observed pots in each fishery (BBRKC
directed and test fisheries are combined), and expanded to total catch
by multiply by the total fishery effort as provided via fish ticket
summaries. Total catch weight (lbs) is estimated by multiplying total
catch number by the average weight of a crab for each subgroup in that
fishery, which is estimated from *measure* pot data. Note: *total catch
estimates are provided here as point estimates without associated
variance, for now*.

``` r
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
```

### Item 5

Raw fish ticket report by statistical area for the most recent directed
snow crab fishery.

``` r
fish_tick %>%
  # add fishery description, filter for sow crab directed fishery
  f_sdr(., col = "fishery", type = "fishery_code") %>%
  filter(target == "snow_crab",
         opening_year == as.numeric(substring(season, 1, 4))) %>%
  # remove cpue, avg_wt, and price fields
  dplyr::select(-cpue, -avg_wt, -price_lbs) %>%
  # save output
  write_csv(here(paste0("snow_crab/output/", season), "item5_snow_crab_fish_ticket_stat_area.csv"))
```
