Bristol Bay Red King Crab Data Summary Example
================

## Data Summary

The following document details an example workflow to produce the
following output:

  - **item1\_total\_catch\_directed\_fishery.csv**- Point estimates of
    total catch (number of crab and pounds) for female, sublegal, and
    total legal crab in the most recent directed BBRKC fishery. Directed
    fishing and cost recovery are combined.
  - **item2\_total\_catch\_tanner\_crab\_e166.csv**- Point estimates of
    total catch (number of crab and pounds) for female, sublegal, and
    total legal crab in the previous season’s Bering Sea Tanner crab
    fishery east of 166 W longitude.
  - **item3\_fish\_ticket\_summary.csv**- Raw fish ticket summary for
    directed BBRKC fishery and BBRKC cost recovery separately.
  - **item4a\_sublegal\_observer\_size\_comp\_directed\_fishery.csv**-
    Carapace length (mm) composition of sublegal males by shell
    condition caught in observer measure pots in the directed BBRKC
    fishery.
  - **item4b\_legal\_observer\_size\_comp\_directed\_fishery.csv**-
    Carapace length (mm) composition of legal males by shell condition
    caught in observer measure pots in the directed BBRKC fishery.
  - **item4a\_female\_observer\_size\_comp\_directed\_fishery.csv**-
    Carapace length (mm) composition of females by shell condition
    caught in observer measure pots in the directed BBRKC fishery.
  - **item5\_retained\_size\_comp.csv**- Carapace length (mm)
    composition of retained catch by shell condition as recorded by
    dockside samplers in the most recent BBRKC directed fishery.
  - **item6a\_sublegal\_observer\_size\_comp\_tanner\_e166\_fishery.csv**-
    Carapace length (mm) composition of sublegal males by shell
    condition caught in observer measure pots in the Bering Sea Tanner
    crab fishery east of 166 W longitude.
  - **item6b\_legal\_observer\_size\_comp\_tanner\_e166\_fishery.csv**-
    Carapace length (mm) composition of legal males by shell condition
    caught in observer measure pots in the Bering Sea Tanner crab
    fishery east of 166 W longitude.
  - **item6c\_female\_observer\_size\_comp\_tanner\_e166\_fishery.csv**-
    Carapace length (mm) composition of females by shell condition
    caught in observer measure pots in the Bering Sea Tanner crab
    fishery east of 166 W longitude.

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
dock <- read_csv(here("bbrkc/data", "RKC-1990-2019_dockside.csv"))

## observer crab detail data
obs_meas <- read_csv(here("bbrkc/data", "RKC-1990-2019_crab_dump.csv"))

## count pot data
pot_sum <- read_csv(here("bbrkc/data", "RKC-1990-2019_potsum.csv"))

## fish ticket data by stat area
ft_files <- list.files(here("misc/data/fish_ticket_summaries"), full.names = T)
c(lapply(grep(".xlsx", ft_files, value = T), f_read_fish_tick_xlsx),
  lapply(ft_files[!grepl(".xlsx", ft_files)], f_read_fish_tick_xlsx, format = "old")) %>%
  do.call("rbind", .) -> fish_tick

## timerseries of directed effort
dir_effort <- read_csv(here("bbrkc/data", "directed_effort_timeseries_DP.csv"))

## parameters for calculated weight estimation
params <- read_csv(here("misc/data", "weight_parameters.csv"))
```

Each RKC dataset is manipulated to achieve the following:

1.  Combine directed IFQ and CDQ, and cost recovery fisheries for
    observer and dockside sampling data  
2.  Filter Tanner crab fisheries in the early 90s to statistical areas
    East of 166 W longitude, and change codes, so that all E166 Tanner
    crab fisheries have the prefix “TT”
3.  Sum fish ticket summary data across stat area. **Note**: *directed
    and cost recovery fisheries are not combined in fish ticket data*.

<!-- end list -->

``` r
# data management ----

## fishery codes for early 90s tanner e166 fisheries
early_90s_tt <- c("EI91", "EI92", paste0("QT", 93:96))

## clean observer and dockside data timeseries
dock %>%
  # combine bbrkc tf and directed fishery, adjust codes for tanner e166 fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) -> dock

obs_meas %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
  # filter EI and QT fisheries in early 90s by stat areas e166
  filter(!(fishery %in% early_90s_tt & (statarea > 660000 | statarea < 0))) %>%
  # combine all tanner e166 fishery codes
  mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery)) -> obs_meas

pot_sum %>%
  # remove added column start_year
  dplyr::select(-start_year) %>%
  # combine bbrkc tf and directed fishery
  mutate(fishery = gsub("XR|CR", "TR", fishery)) %>%
  # filter EI and QT fisheries in early 90s by stat areas e166
  filter(!(fishery %in% early_90s_tt & (statarea > 660000 | statarea < 0))) %>%
  # combine all tanner e166 fishery codes
  mutate(fishery = ifelse(fishery %in% early_90s_tt, gsub("EI|QT", "TT", fishery), fishery)) -> pot_sum

## summarise fish ticket data by fishery
fish_tick %>%
  dplyr::select(-stat_area, -cpue, -avg_wt, -price_lbs) %>%
  group_by(fishery) %>%
  summarise_all(sum, na.rm = T) -> fish_tick_summary
```

### Item 1

Total catch of females, sublegal males, and legal males in the directed
BBRKC fishery (including cost recovery) is estimated from observer count
and measure pot data. Nearly all observer pots in the BBRKC fishery are
measure pots.

To estimate total catch number, totals of each group (females, sublegal
males, and legal males) caught in a fishery are divided by the total
number of observer pot lifts in that fishery (observer CPUE) and
multiplied by the total fishery pot lifts. Total catch weight (lbs) is
estimated by extrapolating the total catch number by the average
individual weight for each group. Average individual weights for each
group are estimated by fishery using observer measure pot data.

``` r
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
  f_average_wt(by = 4, units = "lbs") %>%
  # use sex and legal status to assign group
  mutate(group = case_when(sex == 2 & legal_status == F ~ "female",
                           sex == 1 & legal_status == F ~ "sublegal",
                           sex == 1 & legal_status == T ~ "tot_legal")) %>%
  dplyr::select(fishery, group, avg_wt) %>%
  # join to total catch number and extrapolate
  left_join(total_catch_num, by = c("fishery", "group")) %>%
  mutate(total_catch_lbs = total_catch_num * avg_wt) %>%
  # decipher fishery code, filter for directed fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  filter(substring(fishery, 1, 2) == "TR") %>%
  # remove avg_wt and sex column, and NA groups (sex == 3), arrange by fishery
  ungroup() %>%
  dplyr::select(-avg_wt, -sex) %>%
  filter(!is.na(group)) %>%
  arrange(opening_year) %>%
  write_csv(here(paste0("bbrkc/output/", season), "item1_total_catch_directed_fishery.csv"))
```

### Item 2

Total catch of females, sublegal males, and legal males in the Bering
Sea Tanner crab fishery east of 166 W longitude is estimated from
observer count data. Total catch number is estimated by totaling each
group (females, sublegal males, and legal males) caught in a fishery,
dividing by the number of observer pot lifts in that fishery (observer
CPUE) and then multiplying by the total fishery pot lifts. Total catch
weight (lbs) is estimated by multiplying total catch number by the
average weight for each sex (not each group, *this can be changed if
necessary*). If no directed Tanner crab fishery east of 166 W longitude
took place in the preceeding season, the output will consist of an empty
.csv file with column headers.

``` r
## get direct effort in the tanner e166 fishery
dir_effort %>%
  filter(substring(fishery, 1, 2) == "TT") %>%
  # change directed effort (illegal) in 2017 to 0 (no RKC caught)
  mutate(effort = ifelse(fishery == "TT17", 0, effort)) -> directed_effort

## estimate total bycatch
pot_sum %>%
  # filter for directed E166 tanner crab fisheries
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
  left_join(f_average_wt(obs_meas, by = 1, units = "lbs"), by = c("fishery", "sex")) %>%
  # compute total catch number and weight (lbs)
  mutate(total_catch_num = (count / obs_effort) * effort,
         total_catch_lbs = total_catch_num * avg_wt) %>%
  # replace missing estimates with zeros (closed fishery)
  replace_na(list(total_catch_num = 0,
                  total_catch_lbs = 0)) %>%
  # remove unneeded data
  dplyr::select(-obs_effort, -effort, -sex, -count, -avg_wt) %>%
  # decipher fishery code and filter for past season, arrange by season
  f_sdr(col = "fishery", type = "fishery_code") %>%
  arrange(opening_year) %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item2_total_catch_tanner_crab_e166.csv"))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## `summarise()` regrouping output by 'fishery' (override with `.groups` argument)

### Item 3

The fish ticket report by stat area was summed to fishery scale in the
data management section.

``` r
fish_tick %>%
  # filter for bbrkc directed and cost recovery fishery
  filter(substring(fishery, 1, 2) %in% c("TR", "XR")) %>%
  # decihper fishery code
  f_sdr(col = "fishery", type = "fishery_code") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item3_fish_ticket_summary.csv"))
```

### Item 4

Summed counts of red king crab caught in observer measure pots during
the directed BBRKC fisheries (including cost recovery) by shell
condition in 1 mm size bins for a) sublegal males, b) legal males, and
c) females. Each group is output as a separate .csv file.

``` r
## sublegal males by shell condition
obs_meas %>%
  filter(sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4a_sublegal_observer_size_comp_directed_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  filter(sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4b_legal_observer_size_comp_directed_fishery.csv"))

## females by shell condition
obs_meas %>%
  filter(sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item4c_female_observer_size_comp_directed_fishery.csv"))
```

### Item 5

Summed counts of landed crab (as collected by dockside samplers) by
shell condition in 1 mm size bins for directed BBRKC fishery (including
the cost recovery fishery).

``` r
dock %>%
  f_retained_size_comp(lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[6:ncol(.)])) %>%
  # filter for directed fishery
  filter(substring(fishery, 1, 2) == "TR") %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item5_retained_size_comp.csv"))
```

    ## `summarise()` regrouping output by 'fishery', 'size' (override with `.groups` argument)

### Item 6

Summed counts of red king crab caught in observer measure pots during
the directed Bering Sea Tanner crab fishery east of 166 W longitude by
shell condition in 1 mm size bins for a) sublegal males, b) legal males,
and c) females. Each group is output as a separate .csv file.

``` r
## sublegal males by shell condition
obs_meas %>%
  # filter for directed E166 tanner crab fisheries
  filter(substring(fishery, 1, 2) == "TT",
         sex == 1,legal == 0) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6a_sublegal_observer_size_comp_tanner_e166_fishery.csv"))

## all legal males by shell condition
obs_meas %>%
  # filter for directed E166 tanner crab fisheries
  filter(substring(fishery, 1, 2) == "TT",
         sex == 1, legal %in% c(1, 2, 3, 6)) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6b_legal_observer_size_comp_tanner_e166_fishery.csv"))

## females by shell condition
obs_meas %>%
  # filter for directed E166 tanner crab fisheries
  filter(substring(fishery, 1, 2) == "TT",
         sex == 2) %>%
  f_observer_size_comp(by = 2, lump = F) %>%
  # add a column for total
  mutate(total = rowSums(.[7:ncol(.)])) %>%
  # save output
  write_csv(here(paste0("bbrkc/output/", season), "item6c_female_observer_size_comp_tanner_e166_fishery.csv"))
```
