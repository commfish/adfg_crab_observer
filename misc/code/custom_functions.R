# notes ----
# functions for preparation of adfg bsai crab fishery data

# author: Tyler Jackson
# last update: 7/1/2020 (or see most recent commit)



# dependencies ----
if (!require("tidyverse", quiet = T)) {install.packages("tidyverse"); library(tidyverse)}
if (!require("lubridate", quiet = T)) {install.packages("lubridate"); library(lubridate)}
if (!require("here", quiet = T)) {install.packages("here"); library(here)}
if (!require("readxl", quiet = T)) {install.packages("readxl"); library(readxl)}

# background data for shell condition ----
## shell condition levels
shell_levels <- c("molting", "soft", "new_pliable",
                  "new", "old", "very_old", "very_very_old", "unknown")
# background data for f_sdr function ----
## character 1
tibble(code_1 = c("A", "D", "E", "H", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "C", "X"),
       management_area = c("southeastern_alaska", "yakutat", "prince_william_sound", 
                           "cook_inlet", "kodiak", "chignik", "alaska_peninsula", "norton_cound",
                           "dutch_harbor", "pribilof_islands", "bering_sea", "adak",
                           "st_matthew_island", "bristol_bay", "cdq_fishery", "test_fishery")) -> char_1
## character 2
tibble(code_2 = c("A", "B", "C", "D", "G", "H", "K", "M", "N", "O", "P", "R", "S", "T", "U"),
       target = c("triangle_tanner_crab", "golden_king_crab", "scarlet_king_crab",
                  "dungeness_crab", "grooved_tanner_crab", "hair_crab", "red_and_blue_king_crab",
                  "paralomis_multispina", "snails", "snow_crab", "blue_king_crab",
                  "red_king_crab", "weathervane_scallop", "tanner_crab", "giant_octopus")) -> char_2

## year characters
tibble(code_3 = sprintf("%02d", 0:99),
       opening_year = ifelse(as.numeric(code_3) < 50, 
                             as.numeric(code_3) + 2000, 
                             as.numeric(code_3) + 1900)) -> char_yr
# background data for f_average_wt function ----
params <- read_csv(here::here("misc/data", "weight_parameters.csv"))

# background data for f_legal_status ----
tibble(spcode = c(932, 931, 931, 921),
       fishery_area = c(NA, "E166", "W166", NA),
       legal_size_mm = c(78, 121, 111, 135)) -> legal_size

# f_fish_code_adjust ----
f_fish_code_adjust <- function(x, type) {
  ## get file species
  spp <- unique(x$spcode)
  
  ## rkc dockside file
  if(spp == 921 & type == "dockside"){
    x %>%
      # cdq and test bbrkc rkc fisheries to TR
      mutate(fishery = gsub("XR|CR", "TR", fishery)) %>% 
      # filter EI and QT fisheries in early 90s by stat areas e166
      # filter(!(fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0))) %>%
      # combine all tanner e166 fishery codes
      mutate(fishery = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96))), 
                              paste0("QT", substring(fishery, 3, 4)),
                              fishery)) -> tmp
    # mutate(fishery = ifelse(fishery %in% c("EI91", "EI92", paste0("QT", 93:96)), gsub("EI|QT", "TT", fishery), fishery)) -> tmp
  }
  
  ## snow dockside file
  if(spp == 932 & type == "dockside") {
    x %>%
      # adjust year based on sample date so that fisheries prosecuted in spring get previous year code
      mutate(fishery_adj = ifelse((month(ymd(sampdate)) < 7 & (substring(year(ymd(sampdate)), 3, 4) == substring(fishery, 3, 4))),
                                  paste0(substring(fishery, 1, 2), substring(year(ymd(sampdate)) - 1, 3, 4)),
                                  fishery),
             # fix transition to rationalization yr
             fishery_adj = gsub("QO05r", "QO05", fishery_adj),
             # cdq and eo fisheries to QO
             fishery_adj = gsub("CO|EO", "QO", fishery_adj),
             # bbrkc test fish and cdq fisheries to TR
             fishery_adj = gsub("XR|CR", "TR", fishery_adj),
             # early tanner crab fisheries to QT or TT based on e166 line
             fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96))), 
                                  paste0("QT", substring(fishery_adj, 3, 4)),
                                  fishery_adj)) %>%
      # fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0)),
      #                      paste0("QT", substring(fishery_adj, 3, 4)),
      #                      fishery_adj),
      # fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea <= 660000 | statarea >= 0)),
      #                      paste0("TT", substring(fishery_adj, 3, 4)),
      #                      fishery_adj)) %>%
      # replace fishery with fishery_adj
      mutate(fishery = fishery_adj) %>%
      dplyr::select(-fishery_adj) -> tmp
  }
  
  # tanner dockside file
  if(spp == 931 & type == "dockside"){
    x  %>%
      # adjust year based on sample date so that fisheries prosecuted in spring get previous year code
      mutate(fishery_adj = ifelse((month(ymd(sampdate)) < 7 & substring(year(ymd(sampdate)), 3, 4) == substring(fishery, 3, 4)),
                                  paste0(substring(fishery, 1, 2), substring(year(ymd(sampdate)) - 1, 3, 4)),
                                  fishery),
             # fix transition to rationalization yr
             fishery_adj = gsub("QO05r", "QO05", fishery_adj),
             # cdq and eo fisheries to QO
             fishery_adj = gsub("CO|EO", "QO", fishery_adj),
             # bbrkc test fish and cdq fisheries to TR
             fishery_adj = gsub("XR|CR", "TR", fishery_adj),
             # early tanner crab fisheries to QT or TT based on e166 line
             fishery_adj = ifelse((fishery %in% c("EI89", "EI90", "EI91", "EI92", paste0("QT", 93:96))), 
                                  paste0("QT", substring(fishery_adj, 3, 4)),
                                  fishery_adj)) %>%
      #fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0)),
      # paste0("QT", substring(fishery_adj, 3, 4)),
      # fishery_adj),
      #fishery_adj = ifelse((fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea <= 660000 | statarea >= 0)),
      # paste0("TT", substring(fishery_adj, 3, 4)),
      # fishery_adj)) %>%
      # replace fishery with fishery_adj
      mutate(fishery = fishery_adj) %>%
      dplyr::select(-fishery_adj) -> tmp
  }
  
  ## rkc obs dump files
  if(spp == 921 & type == "obs"){
    x %>%
      # cdq and test bbrkc rkc fisheries to TR
      mutate(fishery = gsub("XR|CR", "TR", fishery)) %>% 
      # filter EI and QT fisheries in early 90s by stat areas e166
      filter(!(fishery %in% c("EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0))) %>%
      # combine all tanner e166 fishery codes
      mutate(fishery = ifelse(fishery %in% c("EI91", "EI92", paste0("QT", 93:96)), gsub("EI|QT", "TT", fishery), fishery)) -> tmp
  }
  
  ## snow obs dump files
  if(spp == 932 & type == "obs") {
    x %>%
      # adjust year based on sample date so that fisheries prosecuted in spring get previous year code
      mutate(fishery_adj = ifelse((month(mdy(sampdate)) < 7 & (substring(year(mdy(sampdate)), 3, 4) == substring(fishery, 3, 4))),
                                  paste0(substring(fishery, 1, 2), substring(year(mdy(sampdate)) - 1, 3, 4)),
                                  fishery),
             # fix transition to rationalization yr
             fishery_adj = gsub("QO05r", "QO05", fishery_adj),
             # cdq and eo fisheries to QO
             fishery_adj = gsub("CO|EO", "QO", fishery_adj),
             # cdq rkc and bkc fisheries to PIBKC
             fishery_adj = gsub("CK", "QP", fishery_adj),
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
  
  # tanner obs dump files
  if(spp == 931 & type == "obs"){
    x  %>%
      # adjust year based on sample date so that fisheries prosecuted in spring get previous year code
      mutate(fishery_adj = ifelse((month(mdy(sampdate)) < 7 & substring(year(mdy(sampdate)), 3, 4) == substring(fishery, 3, 4)),
                                  paste0(substring(fishery, 1, 2), substring(year(mdy(sampdate)) - 1, 3, 4)),
                                  fishery),
             # fix transition to rationalization yr
             fishery_adj = gsub("QO05r", "QO05", fishery_adj),
             # cdq and eo fisheries to QO
             fishery_adj = gsub("CO|EO", "QO", fishery_adj),
             # cdq rkc and bkc fisheries to PIBKC
             fishery_adj = gsub("CK", "QP", fishery_adj),
             # bbrkc test fish and cdq fisheries to TR
             fishery_adj = gsub("XR|CR", "TR", fishery_adj),
             # early tanner crab fisheries to QT or TT based on e166 line
             fishery_adj = ifelse((fishery %in% c("EI89", "EI90", "EI91", "EI92", paste0("QT", 93:96)) & (statarea > 660000 | statarea < 0)),
                                  paste0("QT", substring(fishery_adj, 3, 4)),
                                  fishery_adj),
             fishery_adj = ifelse((fishery %in% c("EI89", "EI90", "EI91", "EI92", paste0("QT", 93:96)) & (statarea <= 660000 | statarea >= 0)),
                                  paste0("TT", substring(fishery_adj, 3, 4)),
                                  fishery_adj)) %>%
      # replace fishery with fishery_adj
      mutate(fishery = fishery_adj) %>%
      dplyr::select(-fishery_adj) -> tmp
  }
  
  tmp
  
}

# f_sdr ----
# custom function for 'uncoding' fishery, shell condition and legal status codes into readily understandable text 
# argument: x   - data frame or tibble containing a code column
#           col - name of column with code. 
#           type - data type (i.e., "fishery_code", "shell_condition", "legal", "sex" code).
f_sdr <- function(x, col, type){
  
  colnames <- names(x)
  
  if(type == "fishery_code") {
    x %>%
      pull(grep(col, names(.))) %>%
      tibble(fishery = .) %>%
      # add codes
      mutate(code_1 = str_sub(fishery, 1, 1),
             code_2 = str_sub(fishery, 2, 2),
             code_3 = str_sub(fishery, 3, 4)) %>%
      # add to textual data
      left_join(char_1, by = "code_1") %>%
      left_join(char_2, by = "code_2") %>%
      left_join(char_yr, by = "code_3") %>%
      # fix issue with historical fishery codes
      mutate(management_area = ifelse((opening_year <= 1995 & code_1 == "E"), 
                                      "eastern_bering_sea", management_area),
             management_area = ifelse((opening_year <= 1995 & code_1 == "W"), 
                                      "western_bering_sea", management_area),
             target = ifelse((opening_year <= 1995 & code_2 == "I"), 
                             "tanner_crab", target)) %>%
      dplyr::select(-fishery, -code_1, -code_2, -code_3) %>%
      bind_cols(x) %>%
      dplyr::select(colnames[grep(col, colnames)], management_area, target, opening_year,
                    colnames[-grep(col, colnames)]) -> tmp
  } 
  if(type == "shell_condition"){
    x %>%
      pull(grep(col, names(.))) %>%
      tibble(shell = .) %>%
      # add text
      mutate(shell_text = case_when(shell == 0 ~ "molting",
                                    shell == 1 ~ "soft", 
                                    shell == 9 ~ "new_pliable",
                                    shell == 2 ~ "new",
                                    shell == 3 ~ "old",
                                    shell == 4 ~ "very_old",
                                    shell == 5 ~ "very_very_old")) %>%
      dplyr::select(-shell) %>%
      bind_cols(x, .) %>%
      dplyr::select(1:grep(col, colnames), shell_text, (grep(col, colnames) + 1):length(colnames)) -> tmp
 
  }
  if(type == "legal"){
    x %>%
      pull(grep(col, names(.))) %>%
      tibble(legal = .) %>%
      # add text
      mutate(legal_text = case_when(legal == 0 ~ "sublegal",
                                    legal == 1 ~ "legal_retained", 
                                    legal == 2 ~ "legal_not_retained",
                                    legal == 3 ~ "legal_illegaly_retained",
                                    legal == 6 ~ "legal_retained_unknown",
                                    legal == -7 ~ "female")) %>%
      dplyr::select(-legal) %>%
      bind_cols(x, .) %>%
      dplyr::select(1:grep(col, colnames), legal_text, (grep(col, colnames) + 1):length(colnames)) -> tmp
  }
  if(type == "sex"){
    x %>%
      pull(grep(col, names(.))) %>%
      tibble(sex = .) %>%
      # add text
      mutate(sex_text = case_when(sex == 0 ~ "unknown",
                                  sex == 1 ~ "male", 
                                  sex == 2 ~ "female",
                                  sex == 3 ~ "hermaphrodite")) %>%
      dplyr::select(-sex) %>%
      bind_cols(x, .) %>%
      dplyr::select(1:grep(col, colnames), sex_text, (grep(col, colnames) + 1):length(colnames)) -> tmp
    
  }
  tmp
}



# f_legal_status ----
# assign legal size status based on sex, size, location (E166, W166 tanner crab)
# args: x - any data frame contains the fields "size", "spcode", and "fishery" (fishery code) for tanner crab only
f_legal_status <- function(x){
  if(unique(x$spcode) == 931){
    x %>%
      mutate(fishery_area = ifelse(substring(fishery, 1, 2) %in% c("TT", "TR", "XR"), 
                                   "E166", "W166")) %>% 
      left_join(legal_size, by = c("spcode", "fishery_area")) %>%
      mutate(legal_size_mm = ifelse(as.numeric(substring(fishery, 3, 4)) %in% c(0:10, 80:99), 140, legal_size_mm),
             legal_status = ifelse((sex == 1 & size >= legal_size_mm), T, F)) %>%
      dplyr::select(-legal_size_mm, -fishery_area) -> tmp
  } else {
    x %>%
      left_join(legal_size, by = c("spcode")) %>%
      mutate(legal_status = ifelse((sex == 1 & size >= legal_size_mm), T, F)) %>%
      dplyr::select(-legal_size_mm, -fishery_area) -> tmp
  }
  tmp
}

# f_retained_size_comp ----
# dockside sampling size composition by shell condition
# argument: x - dockside sampling data for a species by fishery. 
#           lump - T/F. If true, shell codnitions 0, 1, 2 & 9 are "new" and 3 - 5 are "old". Default = F.
f_retained_size_comp <- function(x, lump = F) {
  if(lump == F) {
    x %>%
      filter(!is.na(shell),
             shell != -9) %>%
      group_by(fishery, size, shell) %>%
      summarise(numcrab = sum(numcrab)) %>%
      f_sdr(x = ., col = "shell", type = "shell_condition") %>%
      dplyr::select(-shell) %>%
      pivot_wider(names_from = shell_text, values_from = numcrab) %>%
      replace(is.na(.), 0) %>%
      # add fishery name
      f_sdr(., col = "fishery", type = "fishery_code") %>%
      # reorder columns 
      dplyr::select(1:5, shell_levels[which(shell_levels %in% names(.))]) -> tmp
  } else{
    x %>%
      filter(!is.na(shell),
             shell != -9) %>%
      mutate(shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
                                    shell %in% c(3:5) ~ 3)) %>%
      group_by(fishery, size, shell_lump) %>%
      summarise(numcrab = sum(numcrab)) %>%
      f_sdr(x = ., col = "shell_lump", type = "shell_condition") %>%
      dplyr::select(-shell_lump) %>%
      pivot_wider(names_from = shell_text, values_from = numcrab) %>%
      replace(is.na(.), 0) %>%
      # add fishery name
      f_sdr(., col = "fishery", type = "fishery_code") %>%
      # reorder columns
      dplyr::select(1:5, shell_levels[which(shell_levels %in% names(.))])-> tmp
  }
  tmp
}

# f_observer_size_comp ----
# observer measure pot size composition by sex, shell condition, and/or legal status
# args: x - raw observer measure data for a given species in each fishery it was encountered in
#       by - numeric option denoting which delimiting characteristics to use. 1: sex, 2: sex & shell condition, 3: sex, shell condition & legal status
#       lump - T/F. If true, shell codnitions 0, 1, 2 & 9 are "new" and 3 - 5 are "old". No Default.
#            
f_observer_size_comp <- function(x, by, lump){
  if(by == 1){
    x %>%
      dplyr::select(fishery, sex, size, shell, legal) %>%
      count(fishery, sex, size) %>%
      rename(count = n) %>%
      f_sdr(x =., col = "sex", type = "sex") %>%
      dplyr::select(-sex) %>%
      pivot_wider(names_from = sex_text, values_from = count) %>%
      replace_na(list(male = 0, female = 0, unknown = 0, hermaphrodite = 0)) %>%
      f_sdr(x =., col = "fishery", type = "fishery_code") -> tmp
  }
  if(by == 2 & missing(lump)){stop("Must provide T/F on whether to lump shell conditions into new/old.")}
  if(by == 2 & lump == F){
    x %>%
      # no missing shell condition data
      filter(!is.na(shell),
             shell != -9) %>%
      dplyr::select(fishery, sex, size, shell, legal) %>%
      count(fishery, sex, size, shell) %>%
      rename(count = n) %>%
      f_sdr(x =., col = "shell", type = "shell_condition") %>%
      f_sdr(x = ., col = "sex", type = "sex") %>%
      dplyr::select(-sex, -shell) %>%
      pivot_wider(names_from = shell_text, values_from = count) %>%
      replace_na(list(new = 0, new_pliable = 0, soft = 0, old = 0, 
                      very_old = 0, very_very_old = 0, molting = 0)) %>%
      rename(sex = sex_text) %>%
      f_sdr(x =., col = "fishery", type = "fishery_code") %>%
      # reorder the shell conditions
      dplyr::select(1:6, shell_levels[which(shell_levels %in% names(.))]) -> tmp
  }
  if(by == 2 & lump == T){
    x %>%
      # no missing shell condition data
      filter(!is.na(shell),
             shell != -9) %>%
      mutate(shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
                                    shell %in% c(3:5) ~ 3)) %>%
      dplyr::select(fishery, sex, size, shell_lump, legal) %>%
      count(fishery, sex, size, shell_lump) %>%
      rename(count = n) %>%
      f_sdr(x = ., col = "shell", type = "shell_condition") %>%
      f_sdr(x = ., col = "sex", type = "sex") %>%
      dplyr::select(-sex, -shell_lump) %>%
      pivot_wider(names_from = shell_text, values_from = count) %>%
      replace_na(list(new = 0, old = 0)) %>%
      rename(sex = sex_text) %>%
      f_sdr(x =., col = "fishery", type = "fishery_code") %>%
      # reorder the shell conditions
      dplyr::select(1:6, shell_levels[which(shell_levels %in% names(.))]) -> tmp
  }
  if(by == 3 & lump == F){
    x %>%
      # no missing shell condition data
      filter(!is.na(shell),
             shell != -9) %>%
      dplyr::select(fishery, spcode, sex, size,  shell) %>%
      count(fishery, spcode, sex, size, shell) %>%
      rename(count = n) %>%
      f_sdr(x =., col = "shell", type = "shell_condition") %>%
      f_sdr(x = ., col = "sex", type = "sex") %>%
      f_legal_status() %>%
      dplyr::select(-sex, -shell) %>%
      pivot_wider(names_from = shell_text, values_from = count) %>%
      replace_na(list(new = 0, new_pliable = 0, soft = 0, old = 0, 
                      very_old = 0, very_very_old = 0, molting = 0)) %>%
      rename(sex = sex_text) %>%
      f_sdr(x =., col = "fishery", type = "fishery_code") %>%
      # reorder the shell conditions
      dplyr::select(1:4, 6, 8, 7, shell_levels[which(shell_levels %in% names(.))]) -> tmp
  }
  if(by == 3 & lump == T){
    x %>%
      # no missing shell condition data
      filter(!is.na(shell),
             shell != -9) %>%
      mutate(shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
                                    shell %in% c(3:5) ~ 3)) %>%
      dplyr::select(fishery, spcode, sex, size, shell_lump) %>%
      count(fishery, spcode, sex, size, shell_lump) %>%
      rename(count = n) %>%
      f_sdr(x =., col = "shell_lump", type = "shell_condition") %>%
      f_sdr(x = ., col = "sex", type = "sex") %>%
      f_legal_status() %>%
      dplyr::select(-sex, -shell_lump) %>%
      pivot_wider(names_from = shell_text, values_from = count) %>%
      replace_na(list(new = 0,  old = 0)) %>%
      rename(sex = sex_text) %>%
      f_sdr(x =., col = "fishery", type = "fishery_code") %>%
      # reorder the shell conditions
      dplyr::select(1:4, 6, 8, 7, shell_levels[which(shell_levels %in% names(.))]) -> tmp
  }
  tmp
}


# f_average_wt ----
# get average weight by a crab (by sex and maturity status) in each fishery in which it was 
# encountered. Codes are left as is. Output is meant to be joined for data pipeline using codes.
# If grouping by sex and shell condition, shell condition is ALWAYS lumped to new and old.
# args: x - raw observer measure pot data for a given species in each fishery it was encountered in
#       by - numeric option denoting which delimiting characteristics to use. 1: sex, 2: sex and shell condition, 3: sex, shell condition, and legal status, 4: sex and legal status
#       legal_code - Logical. If TRUE, legal designations are based on observer stick measures (1 / 0) instead of size, species, and location (Tanner crab)
#       units - "kg" or "lbs". Default = "kg"
f_average_wt <- function(x, by, legal_code = T, units = "kg"){

  ## add maturity group text column
  ### RKC, BKC, GKC
  if(unique(x$spcode) %in% 921:923){
    x %>%
    mutate(maturity = case_when(sex == 1 ~ "male",
                                sex == 2 & size >= 90 ~ "mature", 
                                sex == 2 & size < 90 ~ "immature")) -> x
  } 
  ### snow and tanner crab
  if(unique(x$spcode) %in% 931:932){
    x %>%
      mutate(maturity = case_when(sex == 1 ~ "male",
                                  sex == 2 & (maturity == 1 | clutch > 0) ~ "mature", 
                                  sex == 2 & (maturity == 0 | clutch <= 0) ~ "immature")) -> x
  }
  
  if(by == 1){
    x %>%
      # remove females that are maturity information
      filter(!(sex == 2 & is.na(maturity))) %>%
      dplyr::select(fishery, sex, spcode, size, shell, maturity) %>%
      count(fishery, sex, spcode, size, maturity) %>%
      rename(count = n) %>%
      # join to growth parameters
      left_join(params, by = c("sex", "spcode", "maturity")) %>%
      # estimate calculated weight
      mutate(calc_wt_kg = (alpha * as.numeric(size)^beta) / 1000) %>%
      group_by(fishery, sex) %>%
      summarise(avg_wt = weighted.mean(calc_wt_kg, w = count)) -> tmp
  }
  if(by == 2){
    x %>%
      # remove females that are missing maturity information and all individuals missing shell height information
      filter(!(sex == 2 & is.na(maturity)),
             !is.na(shell),
             shell != -9) %>%
      mutate(shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
                                    shell %in% c(3:5) ~ 3)) %>%
      dplyr::select(fishery, sex, spcode, size, shell_lump, maturity) %>%
      count(fishery, sex, spcode, size, shell_lump, maturity) %>%
      rename(count = n) %>%
      # join to growth parameters
      left_join(params, by = c("sex", "spcode", "maturity")) %>%
      # estimate calculated weight
      mutate(calc_wt_kg = (alpha * as.numeric(size)^beta) / 1000) %>%
      group_by(fishery, sex, shell_lump) %>%
      summarise(avg_wt = weighted.mean(calc_wt_kg, w = count)) -> tmp
  }
  if(by == 3){
    if(legal_code == T){
      x %>%
        # remove females that are missing maturity information and all individuals missing shell height information
        filter(!(sex == 2 & is.na(maturity)),
               !is.na(shell),
               shell != -9,
               !is.na(legal)) %>%
        # fix legal designation
        mutate(legal_status = case_when(legal %in% c(0, -7) ~ 0,
                                      legal %in% 1:6 ~ 1),
               legal_status = ifelse(legal_status == -9, NA, legal_status),
               shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
                                      shell %in% c(3:5) ~ 3)) %>%
        filter(!is.na(legal_status)) %>%
        count(fishery, sex, spcode, size, shell_lump, maturity, legal_status) %>%
        rename(count = n) %>%
        # join to growth parameters
        left_join(params, by = c("sex", "spcode", "maturity")) %>%
        # estimate calculated weight
        mutate(calc_wt_kg = (alpha * as.numeric(size)^beta) / 1000) %>%
        group_by(fishery, sex, shell_lump, legal_status) %>%
        summarise(avg_wt = weighted.mean(calc_wt_kg, w = count)) %>%
        mutate(legal_status = legal_status == 1) -> tmp
    }
    if(legal_code == F){
      x %>%
        # remove females that are missing maturity information and all individuals missing shell height information
        filter(!(sex == 2 & is.na(maturity)),
               !is.na(shell),
               shell != -9) %>%
        mutate(shell_lump = case_when(shell %in% c(0:2, 9) ~ 2,
                                      shell %in% c(3:5) ~ 3)) %>%
        dplyr::select(fishery, sex, spcode, size, shell_lump, maturity) %>%
        f_legal_status() %>%
        count(fishery, sex, spcode, size, shell_lump, maturity, legal_status) %>%
        rename(count = n) %>%
        # join to growth parameters
        left_join(params, by = c("sex", "spcode", "maturity")) %>%
        # estimate calculated weight
        mutate(calc_wt_kg = (alpha * as.numeric(size)^beta) / 1000) %>%
        group_by(fishery, sex, shell_lump, legal_status) %>%
        summarise(avg_wt = weighted.mean(calc_wt_kg, w = count)) -> tmp
    }
  }
  
  if(by == 4){
    if(legal_code == T){
      x %>%
        # remove females that are missing maturity information and all individuals missing shell height information
        filter(!(sex == 2 & is.na(maturity)),
               !is.na(legal)) %>%
        # fix legal designation
        mutate(legal_status = case_when(legal %in% c(0, -7) ~ 0,
                                        legal %in% 1:6 ~ 1),
               legal_status = ifelse(legal_status == -9, NA, legal_status),) %>%
        filter(!is.na(legal_status)) %>%
        count(fishery, sex, spcode, size, maturity, legal_status) %>%
        rename(count = n) %>%
        # join to growth parameters
        left_join(params, by = c("sex", "spcode", "maturity")) %>%
        # estimate calculated weight
        mutate(calc_wt_kg = (alpha * as.numeric(size)^beta) / 1000) %>%
        group_by(fishery, sex, legal_status) %>%
        summarise(avg_wt = weighted.mean(calc_wt_kg, w = count)) -> tmp
    }
    if(legal_code == F){
      x %>%
        # remove females that are maturity information
        filter(!(sex == 2 & is.na(maturity))) %>%
        dplyr::select(fishery, sex, spcode, size, maturity) %>%
        f_legal_status() %>%
        count(fishery, sex, spcode, size, maturity, legal_status) %>%
        rename(count = n) %>%
        # join to growth parameters
        left_join(params, by = c("sex", "spcode", "maturity")) %>%
        # estimate calculated weight
        mutate(calc_wt_kg = (alpha * as.numeric(size)^beta) / 1000) %>%
        group_by(fishery, sex, legal_status) %>%
        summarise(avg_wt = weighted.mean(calc_wt_kg, w = count)) -> tmp
    }
  }
  if(units == "lbs"){
    tmp$avg_wt <- tmp$avg_wt * 2.2046226218
  }
  tmp
}


# f_sday ----
# coerce sample date into day of season.
# args: x - sample date in the format MM-/DD-/YYYY
#       y - Julian date for start of season in a non-leap year. Default is October 15, 288.
f_sday <- function(x, y = 288){
  case_when((year(mdy(x)) %in% seq(0, 100000, 4) & yday(mdy(x)) > (y)) ~ yday(mdy(x)) - (y),
            (year(mdy(x)) %in% seq(0, 100000, 4) & yday(mdy(x)) <= (y)) ~ yday(mdy(x)) + 366 - (y),
            (!(year(mdy(x)) %in% seq(0, 100000, 4)) & yday(mdy(x)) > (y - 1)) ~ yday(mdy(x)) - (y - 1),
            (!(year(mdy(x)) %in% seq(0, 100000, 4)) & yday(mdy(x)) <= (y - 1))  ~ yday(mdy(x)) + 365 - (y - 1))
}
  
# f_stat_week ----
# assign ADF&G statistical week to date
# args: x - sample date in the format MM-/DD-/YYYY
f_stat_week <- function(x){
  case_when(epiweek(mdy(paste0("1-1-", year(mdy(x))))) > 50 ~ ifelse(month(mdy(x)) == 1 & epiweek(mdy(x)) > 50, 1, epiweek(mdy(x)) + 1),
            epiweek(mdy(paste0("1-1-", year(mdy(x))))) == 1 ~ epiweek(mdy(x)))
  
}

# f_read_fish_tick_xlsx ----
# read excel sheet fish ticket reports by stat_area provided by Dutch Harbor ADF&G
# args: path - file path to excel document

f_read_fish_tick_xlsx <- function(path, format = "new") {
  
  ## function to map across sheets for cleaning up data
  f_ft_clean <- function(x) {
    if(nrow(x) > 0){
      row_num <- grep("Total", pull(x, 1))
      x %>%
        slice(-c(row_num:nrow(x))) %>%
        mutate_all(as.character())
    }
  }
  
  if(format == "new"){
    suppressWarnings( 
      tibble(fishery = excel_sheets(path),
             xl_list = lapply(excel_sheets(path), function(x) read_excel(path = path, sheet = x, skip = 2, col_types = "text"))) %>%
        mutate(n_row = purrr::map_dbl(xl_list, nrow),
               xl_list = purrr::map(xl_list, f_ft_clean)) %>%
        # filter data for only single fisheries that have data (assuming 4 char is fishery code)
        filter(nchar(fishery, type = "chars") == 4, 
               n_row > 0) %>%
        dplyr::select(-n_row) %>%
        unnest(xl_list) %>%
        # rename columns
        rename_all(~c("fishery", "stat_area", "vessels", "landings", "live_number", "live_lbs", "deadloss_number", "deadloss_lbs", "effort", "cpue", "avg_wt", "price_lbs")) %>%
        # fix column class
        mutate_at(3:12, as.numeric)
    ) -> tmp
  }
  
  
  if(format == "old"){
    suppressWarnings( 
    tibble(fishery = excel_sheets(path),
           xl_list = lapply(excel_sheets(path), function(x) read_excel(path = path, sheet = x, skip = 11, col_types = "text"))) %>%
      mutate(n_row = purrr::map_dbl(xl_list, nrow),
             xl_list = purrr::map(xl_list, f_ft_clean)) %>%
      # filter data for only single fisheries that have data (assuming 4 char is fishery code)
      filter(nchar(fishery, type = "chars") == 4, 
             n_row > 0) %>%
      dplyr::select(-n_row) %>%
      unnest(xl_list) %>%
      # rename columns
      rename_all(~c("fishery", "stat_area", "vessels", "landings", "live_number", "live_lbs", "deadloss_number", "deadloss_lbs", "effort", "cpue", "avg_wt", "price_lbs")) %>%
      # remove blank rows
      filter(!is.na(stat_area)) %>%
      # fix column class
      mutate_at(3:12, as.numeric)
    ) -> tmp
  }
  tmp
}

  