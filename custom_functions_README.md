Custom Functions and Background Data
================

## Purpose

Custom functions are used throughout the workflows of each data summary
to reduce clutter caused by routine tasks and simple data manipulation.
Below is a brief overview and example usage of each custom function, as
well as their associated background data.

## Dependencies

By sourcing the R script `custom_functions.R` the packages `tidyverse`
and `here` are loaded.

## Functions

### Secret Decoder Ring `f_sdr()`

This function deciphers ADF\&G data codes, currently including fishery,
shell condition and legal codes into readily understandable text. The
inputs is a data frame containing a column for one of the previously
mentioned codes, and the output is that data frame with a textual column
immediately succeeding the code coulmn. Note: Tanner crab fisheies east
of E166 will decipher the management area to be Bristol Bay, since the
area code T techinically refers to Bristol Bay. Tanner crab fisheies
east of W166 will decipher the management area to be Bering Sea.

Arguments:

  - **x** - data frame or tibble containing a code column  
  - **col** - name of column with code, as a character string (example:
    “fishery”)  
  - **type** - type of code to decipher (i.e., “fishery\_code”,
    “shell\_condition”, “legal”, “sex”)

Usage: Example using fishery code for dockside sampling data

``` r
f_sdr(x = dock, col = "fishery", type = "fishery_code")
```

    ## # A tibble: 11,218 x 11
    ##    fishery management_area target opening_year  adfg sampdate   spcode  size
    ##    <chr>   <chr>           <chr>         <dbl> <dbl> <date>      <dbl> <dbl>
    ##  1 QO19    bering_sea      snow_~         2019     7 2020-02-09    932    92
    ##  2 QO19    bering_sea      snow_~         2019     7 2020-02-09    932    96
    ##  3 QO19    bering_sea      snow_~         2019     7 2020-02-09    932    98
    ##  4 QO19    bering_sea      snow_~         2019     7 2020-02-09    932    99
    ##  5 QO19    bering_sea      snow_~         2019     7 2020-02-09    932   100
    ##  6 QO19    bering_sea      snow_~         2019     7 2020-02-09    932   101
    ##  7 QO19    bering_sea      snow_~         2019     7 2020-02-09    932   102
    ##  8 QO19    bering_sea      snow_~         2019     7 2020-02-09    932   103
    ##  9 QO19    bering_sea      snow_~         2019     7 2020-02-09    932   104
    ## 10 QO19    bering_sea      snow_~         2019     7 2020-02-09    932   105
    ## # ... with 11,208 more rows, and 3 more variables: legal <dbl>, shell <dbl>,
    ## #   numcrab <dbl>

### Legal Status `f_legal_status()`

This function creates a column of logical values (T/F) denoting whether
a crab would be considered legal size based on sex, carapace width or
length (by species and management area).

Arguments:

  - **x** - any data frame containing the fields “size”, “spcode”, and
    “fishery” (fishery code; Tanner crab only). Columns must be
    present in code form.

Usage: Example using observer measure pot sampling data

``` r
f_legal_status(x = obs_meas) %>%
  dplyr::select(fishery, spcode, sex, legal_status, size)
```

    ## # A tibble: 337,603 x 5
    ##    fishery spcode   sex legal_status  size
    ##    <chr>    <dbl> <dbl> <lgl>        <dbl>
    ##  1 QO19       932     2 FALSE           73
    ##  2 QO19       932     2 FALSE           60
    ##  3 QO19       932     2 FALSE           58
    ##  4 QO19       932     1 TRUE            85
    ##  5 QO19       932     1 TRUE            98
    ##  6 QO19       932     1 TRUE            97
    ##  7 QO19       932     1 TRUE           105
    ##  8 QO19       932     1 TRUE           106
    ##  9 QO19       932     1 TRUE           100
    ## 10 QO19       932     1 TRUE            91
    ## # ... with 337,593 more rows

### Retained Catch Size Composition `f_retained_size_comp`

This function summarise dockside sampling data to produce size
composition data for crab retained in a fishery, by shell condition. The
output is a data frame containing information on the fishery, carapace
width (or length) (mm), and shell condition (either lumped as “new” and
“old” or for individual codes).

Arguments:

  - **x** - dockside sampling data for a species by fishery.  
  - **lump** - option T/F. If TRUE, shell conditions 0, 1, 2, and 9 are
    summed as “new” and shell conditions 3, 4, and 5 are summed as
    “old”. Default = FALSE.

Usage: Example using dockside sampling data

``` r
f_retained_size_comp(x = dock, lump = T) 
```

    ## `summarise()` regrouping output by 'fishery', 'size' (override with `.groups` argument)

    ## # A tibble: 68 x 7
    ##    fishery management_area target    opening_year  size   new   old
    ##    <chr>   <chr>           <chr>            <dbl> <dbl> <dbl> <dbl>
    ##  1 QO19    bering_sea      snow_crab         2019    72     0     1
    ##  2 QO19    bering_sea      snow_crab         2019    74     1     0
    ##  3 QO19    bering_sea      snow_crab         2019    75     0     1
    ##  4 QO19    bering_sea      snow_crab         2019    79     2     0
    ##  5 QO19    bering_sea      snow_crab         2019    80     1     1
    ##  6 QO19    bering_sea      snow_crab         2019    81     2     0
    ##  7 QO19    bering_sea      snow_crab         2019    82     1     1
    ##  8 QO19    bering_sea      snow_crab         2019    83     0     2
    ##  9 QO19    bering_sea      snow_crab         2019    84     3     1
    ## 10 QO19    bering_sea      snow_crab         2019    85     4     0
    ## # ... with 58 more rows

### Observer Sampling Size Composition `f_observer_size_comp`

This function summarise observer measure pot data to produce size
composition data for crab caught in a fishery, by sex, shell condition,
and/or legal status (T/F based on size, see `f_legal_status`. Shell
condition can be lumped into “new” or “old” broad catagories, or left as
specific shell conditions. The output contains a line for each unique
combination of sex, shell condition, and/or lega status by fishery and a
count of crab.

Arguments:

  - **x** - observer measure pot data for a species by fishery.  
  - **by** - numeric option denoting which delimiting characteristics to
    use. 1: sex, 2: sex & shell condition, 3: sex, shell condition &
    legal status
  - **lump** - option T/F. If TRUE, shell conditions 0, 1, 2, and 9 are
    summed as “new” and shell conditions 3, 4, and 5 are summed as
    “old”. No default, must be provided.

Usage: Example using observer measure pot data

``` r
f_observer_size_comp(x = obs_meas, by = 2, lump = T)
```

    ## # A tibble: 139 x 8
    ##    fishery management_area target    opening_year sex    size   new   old
    ##    <chr>   <chr>           <chr>            <dbl> <chr> <dbl> <dbl> <dbl>
    ##  1 QO19    bering_sea      snow_crab         2019 male     48     1     0
    ##  2 QO19    bering_sea      snow_crab         2019 male     51     1     0
    ##  3 QO19    bering_sea      snow_crab         2019 male     53     4     0
    ##  4 QO19    bering_sea      snow_crab         2019 male     54     3     0
    ##  5 QO19    bering_sea      snow_crab         2019 male     55     5     0
    ##  6 QO19    bering_sea      snow_crab         2019 male     56     4     3
    ##  7 QO19    bering_sea      snow_crab         2019 male     57     5     3
    ##  8 QO19    bering_sea      snow_crab         2019 male     58    10     7
    ##  9 QO19    bering_sea      snow_crab         2019 male     59    25     3
    ## 10 QO19    bering_sea      snow_crab         2019 male     60    25     9
    ## # ... with 129 more rows

### Average Crab Weight `f_average_wt`

This function estimates the average weight of a crab in a fishery by
sex, shell contion, and/or legal status based on observer measure pot
data and parameters (\(\alpha\) and \(\beta\)) of an allometric growth
function based on NMFS survey data. Shell condition is **always** lumped
into “new” and “old” catagories. Calculated weight (g) is estimated as

\[W = \alpha L ^\beta\],

and average weight is estimated as a weighted mean of calculated weight
with count of crab as the weight. Codes are left as is. Output is meant
to be joined for data pipeline using codes.

Arguments:

  - **x** - observer measure pot data for a species by fishery.  
  - **by** - numeric option denoting which delimiting characteristics to
    use. 1: sex, 2: sex and shell condition, 3: sex, shell condition,
    and legal status, 4: sex and legal status.
  - **legal\_code** - Logical. If TRUE, legal designations are based on
    observer stick measures (1 / 0) instead of size, species, and
    location (Tanner crab). Default = T.
  - **units** - “kg” or “lbs”. Default = “kg”.

Usage: Example using observer measure pot data

``` r
f_average_wt(x = obs_meas, by = 4, legal_code = T, units = "lbs")
```

    ## `summarise()` regrouping output by 'fishery', 'sex' (override with `.groups` argument)

    ## # A tibble: 2 x 4
    ## # Groups:   fishery, sex [1]
    ##   fishery   sex legal_status avg_wt
    ##   <chr>   <dbl>        <dbl>  <dbl>
    ## 1 QO19        1            0  0.367
    ## 2 QO19        1            1  0.892

### Day of Season `f_sday`

This function coerces sample date in the format “MMDDYYYY” separated by
“-” or “/” (ex: “7/23/2020”, “7-23-2020”) into date of the BSAI
commercial fishing season. By default, the first day of the season is
October 15th, but season start date can be any day of the year.

Arguments:

  - **x** - sample date in the format MM-/DD-/YYYY.
  - **y** - Julian date for start of season in a non-leap year. Default
    is October 15, y = 288.

Usage: Example using observer measure pot data

``` r
obs_meas %>%
  dplyr::select(fishery, sampdate) %>%
  mutate(season_day = f_sday(sampdate))
```

    ## # A tibble: 337,603 x 3
    ##    fishery sampdate   season_day
    ##    <chr>   <chr>           <dbl>
    ##  1 QO19    01-04-2020         82
    ##  2 QO19    01-04-2020         82
    ##  3 QO19    01-04-2020         82
    ##  4 QO19    01-04-2020         82
    ##  5 QO19    01-04-2020         82
    ##  6 QO19    01-04-2020         82
    ##  7 QO19    01-04-2020         82
    ##  8 QO19    01-04-2020         82
    ##  9 QO19    01-04-2020         82
    ## 10 QO19    01-04-2020         82
    ## # ... with 337,593 more rows

### Stat Week `f_stat_week`

This function assigns ADF\&G stat week to sample date in the format
“MMDDYYYY” separated by “-” or “/” (ex: “7/23/2020”, “7-23-2020”).

Arguments:

  - **x** - sample date in the format MM-/DD-/YYYY.

Usage: Example using observer measure pot data

``` r
obs_meas %>%
  dplyr::select(fishery, sampdate) %>%
  mutate(stat_week = f_stat_week(sampdate))
```

    ## # A tibble: 337,603 x 3
    ##    fishery sampdate   stat_week
    ##    <chr>   <chr>          <dbl>
    ##  1 QO19    01-04-2020         1
    ##  2 QO19    01-04-2020         1
    ##  3 QO19    01-04-2020         1
    ##  4 QO19    01-04-2020         1
    ##  5 QO19    01-04-2020         1
    ##  6 QO19    01-04-2020         1
    ##  7 QO19    01-04-2020         1
    ##  8 QO19    01-04-2020         1
    ##  9 QO19    01-04-2020         1
    ## 10 QO19    01-04-2020         1
    ## # ... with 337,593 more rows
