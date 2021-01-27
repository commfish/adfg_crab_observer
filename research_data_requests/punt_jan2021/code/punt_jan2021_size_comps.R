# notes ----
## size composition of big-3 fisheries
## Tyler Jackson
## 1/27/2021

# load ----
source("misc/code/custom_functions.R")

# custom function
f_fish_code_adjust <- function(x) {
  ## get file species
  spp <- unique(x$spcode)
  
  ## rkc file
  if(spp == 921){
    x %>%
      # cdq and test bbrkc rkc fisheries to TR
      mutate(fishery = gsub("XR|CR", "TR", fishery)) %>% 
      # filter EI and QT fisheries in early 90s by stat areas e166
      filter(!(fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0))) %>%
      # combine all tanner e166 fishery codes
      mutate(fishery = ifelse(fishery %in% c("EI91", "EI92", paste0("QT", 93:96)), gsub("EI|QT", "TT", fishery), fishery)) -> tmp
  }
  
  ## snow file
  if(spp == 932) {
    x %>%
      # adjust year based on sample date so that fisheries prosecuted in spring get previous year code
      mutate(fishery_adj = ifelse((month(mdy(sampdate)) < 7 & substring(year(mdy(sampdate)), 3, 4) == substring(fishery, 3, 4)),
                                  paste0(substring(fishery, 1, 2), substring(year(mdy(sampdate)) - 1, 3, 4)),
                                  fishery),
             # fix transition to rationalization yr
             fishery_adj = gsub("QO05r", "QO05", fishery_adj),
             # cdq and eo fisheries to QO
             fishery_adj = gsub("CO|EO", "QO", fishery_adj),
             # bbrkc test fish and cdq fisheries to TR
             fishery_adj = gsub("XR|CR", "TR", fishery_adj),
             # early tanner crab fisheries to QT or TT based on e166 line
             fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0)),
                                  paste0("QT", substring(fishery_adj, 3, 4)),
                                  fishery_adj),
             fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea <= 660000 | statarea >= 0)),
                                  paste0("TT", substring(fishery_adj, 3, 4)),
                                  fishery_adj)) %>%
      # replace fishery with fishery_adj
      mutate(fishery = fishery_adj) %>%
      dplyr::select(-fishery_adj) -> tmp
  }
  
  # tanner file
  if(spp == 931){
    x  %>%
      # adjust year based on sample date so that fisheries prosecuted in spring get previous year code
      mutate(fishery_adj = ifelse((month(mdy(sampdate)) < 7 & substring(year(mdy(sampdate)), 3, 4) == substring(fishery, 3, 4)),
                                  paste0(substring(fishery, 1, 2), substring(year(mdy(sampdate)) - 1, 3, 4)),
                                  fishery),
             # fix transition to rationalization yr
             fishery_adj = gsub("QO05r", "QO05", fishery_adj),
             # cdq and eo fisheries to QO
             fishery_adj = gsub("CO|EO", "QO", fishery_adj),
             # bbrkc test fish and cdq fisheries to TR
             fishery_adj = gsub("XR|CR", "TR", fishery_adj),
             # early tanner crab fisheries to QT or TT based on e166 line
             fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0)),
                                  paste0("QT", substring(fishery_adj, 3, 4)),
                                  fishery_adj),
             fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea <= 660000 | statarea >= 0)),
                                  paste0("TT", substring(fishery_adj, 3, 4)),
                                  fishery_adj)) %>%
      # replace fishery with fishery_adj
      mutate(fishery = fishery_adj) %>%
      dplyr::select(-fishery_adj) -> tmp
  }
 tmp
}

# data ----

## list all crab dump files - 1) rkc, 2) snow, 3) tanner
dump_ls <- tibble(file = list.files("research_data_requests/punt_jan2021/raw_data"),
                  data = lapply(grep("crab_dump", list.files("research_data_requests/punt_jan2021/raw_data", full.names = T), value = T),
                                read_csv))

# data management ----

## separate files and adjust fishery codes (see above)
### bbrkc
bbrkc <- f_fish_code_adjust(dump_ls$data[[1]])
### snow
snow <- f_fish_code_adjust(dump_ls$data[[2]])
### tanner 
tanner <- f_fish_code_adjust(dump_ls$data[[3]])


## bbrkc size comp ----

bbrkc %>%
  # generate size comp
  f_observer_size_comp(by = 1, lump = T) %>%
  # retain only directed fishery, snow fishery, and tanner fisheries
  filter(substring(fishery, 1, 2) %in% c("TR", "QO", "QT", "TT")) %>%
  # combine tanner crab fisheries by year
  mutate(fishery = gsub("TT", "QT", fishery)) %>%
  group_by(fishery, size) %>%
  summarise(n_male = sum(male, na.rm = T)) %>%
  # get rid of zero counts (come from having females of that size)
  filter(n_male > 0) %>%
  # pivot to wide format by fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  dplyr::select(-management_area, -fishery) %>%
  pivot_wider(names_from = target, values_from = n_male) %>%
  # filter for seasons after 1997
  filter(opening_year >= 1997) %>%
  # add bbrkc caught in snow cab fishery (none)
  # convert opening year to season
  mutate(snow_crab = 0,
         opening_year = paste0(opening_year, "-", substring(opening_year + 1, 3, 4))) %>%
  # change NA in counts to 0
  replace_na(list(snow_crab = 0,
                  red_king_crab = 0,
                  tanner_crab = 0)) %>%
  # rename columns
  rename(Season = opening_year,
         Pcallmale_inPcfishery_number = red_king_crab,
         Pcallmale_inCbfishery_number = tanner_crab,
         Pcallmale_inCofishery_number = snow_crab) %>%
  ungroup() %>%
  arrange(Season) %>%
  write_csv(here("research_data_requests/punt_jan2021/output", "bbrkc_size_comp.csv"))
         
## snow size comp ----  
  
snow %>%
  # generate size comp
  f_observer_size_comp(by = 1, lump = T) %>%
  # retain only directed fishery, snow fishery, and tanner fisheries
  filter(substring(fishery, 1, 2) %in% c("TR", "QO", "QT", "TT")) %>%
  # combine tanner crab fisheries by year
  mutate(fishery = gsub("TT", "QT", fishery)) %>%
  group_by(fishery, size) %>%
  summarise(n_male = sum(male, na.rm = T)) %>%
  # get rid of zero counts (come from having females of that size)
  filter(n_male > 0) %>%
  # pivot to wide format by fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  dplyr::select(-management_area, -fishery) %>%
  pivot_wider(names_from = target, values_from = n_male) %>%
  # filter for seasons after 1997
  filter(opening_year >= 1997) %>%
  # convert opening year to season
  mutate(opening_year = paste0(opening_year, "-", substring(opening_year + 1, 3, 4))) %>%
  # change NA in counts to 0
  replace_na(list(snow_crab = 0,
                  red_king_crab = 0,
                  tanner_crab = 0)) %>%
  # rename columns
  rename(Season = opening_year,
         Coallmale_inPcfishery_number = red_king_crab,
         Coallmale_inCbfishery_number = tanner_crab,
         Coallmale_inCofishery_number = snow_crab) %>%
  ungroup() %>%
  arrange(Season) %>%
  write_csv(here("research_data_requests/punt_jan2021/output", "snow_size_comp.csv"))

## tanner size comp ----  

tanner %>%
  # generate size comp
  f_observer_size_comp(by = 1, lump = T) %>%
  # retain only directed fishery, snow fishery, and tanner fisheries
  filter(substring(fishery, 1, 2) %in% c("TR", "QO", "QT", "TT")) %>%
  # combine tanner crab fisheries by year
  mutate(fishery = gsub("TT", "QT", fishery)) %>%
  group_by(fishery, size) %>%
  summarise(n_male = sum(male, na.rm = T)) %>%
  # get rid of zero counts (come from having females of that size)
  filter(n_male > 0) %>%
  # pivot to wide format by fishery
  f_sdr(col = "fishery", type = "fishery_code") %>%
  dplyr::select(-management_area, -fishery) %>%
  pivot_wider(names_from = target, values_from = n_male) %>%
  # filter for seasons after 1997
  filter(opening_year >= 1997) %>%
  # convert opening year to season
  mutate(opening_year = paste0(opening_year, "-", substring(opening_year + 1, 3, 4))) %>%
  # change NA in counts to 0
  replace_na(list(snow_crab = 0,
                  red_king_crab = 0,
                  tanner_crab = 0)) %>%
  # rename columns
  rename(Season = opening_year,
         Cballmale_inPcfishery_number = red_king_crab,
         Cballmale_inCbfishery_number = tanner_crab,
         Cballmale_inCofishery_number = snow_crab) %>%
  ungroup() %>%
  arrange(Season) %>%
  write_csv(here("research_data_requests/punt_jan2021/output", "tanner_size_comp.csv"))

  
