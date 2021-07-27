# notes ----

## BBRKC cpue standardization
## Tyler Jackson
## 7/21/2021

# load ----

library(tidyverse)
library(lubridate)
library(MASS)
library(splines)
library(scales)
source("./misc/code/custom_functions.R")
source("./misc/code/adfg_map_functions.R")

# ggplot options (theme and minor axis ticks)
theme_set(FNGr::theme_sleek())
yr_axis <- FNGr::tickr(tibble(yr = 1990:2020), yr, 5)

# data ----

## observer count pount data
tibble(potsum = purrr::map(grep("potSummary", list.files("./research_projects/bbrkc_cpue/data", full.names = T), value = T), read.csv)) %>%
  unnest(potsum) %>%
  rename_all(tolower) %>%
  rename(e_w = e.w) %>%
  as_tibble() -> pot_sum

## fish ticket data
read_csv("./research_projects/bbrkc_cpue/data/BBRKC DataDump - 1990 -2021-FINAL.csv", guess_max = 1e6) %>%
  rename_all(.funs = function(x){gsub("_\\(sum\\)", "", gsub(" ", "_", tolower(x)))}) %>%
  as_tibble() -> ft_dump

# stat area shapefile
f_shp_prep(path = here::here("misc/data/adfg_stat_area"), layer = "StatAreas") %>%
  f_transform_crs(to = proj4string(raster::getData("GADM", country = c("USA"), 
                                                   level = 1, path = "./misc/data"))) %>%
  .[[1]] %>%
  dplyr::select(1:8) %>%
  rename(stat_area = STAT_AREA) %>%
  mutate(stat_area = as.character(stat_area)) -> stat_area


# core vessels ----

## identify core vessels
ft_dump %>%
  mutate(season = as.numeric(substring(season, 1, 4))) %>%
  group_by(season, adfg_number) %>%
  summarise(effort = sum(effort, na.rm = T),
            n_trips = length(unique(date_fishing_began))) %>%
  group_by(season) %>%
  arrange(season, -effort) %>%
  mutate(cum_prop = effort / sum(effort),
         cum_prop = cumsum(cum_prop)) %>%
  # remove vessels not accounting for top 95% of catch
  filter(cum_prop <= 0.50) %>%
  group_by(adfg_number) %>%
  nest() %>%
  mutate(nyr = purrr::map_dbl(data, nrow)) %>%
  # remove vessels not present in 1/ of the years
  filter(nyr >= 10) %>%
  pull(adfg_number) -> core_vessels

## plot of proportion of retained catch
ft_dump %>%
  mutate(season = as.numeric(substring(season, 1, 4))) %>%
  filter(adfg_number %in% core_vessels) %>%
  group_by(season) %>%
  summarise(core_retained = sum(landed_weight, na.rm = T)) %>%
  left_join(ft_dump %>%
              group_by(season) %>%
              mutate(season = as.numeric(substring(season, 1, 4))) %>%
              summarise(all_retained = sum(landed_weight, na.rm = T))) %>%
  mutate(prop_ret = core_retained / all_retained) %>%
  
  mutate(noncore_retained = all_retained - core_retained) %>%
  pivot_longer(c(core_retained, noncore_retained)) %>%
  mutate(name = ifelse(name == "core_retained", "Core Vessels", "Non-core Vessels")) %>%
  ggplot()+
  geom_bar(aes(x = season, y = value / 1000000, fill = name), position = "stack", stat = "identity", color = 1, width = 1)+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Retained Catch (mil lb)", fill = NULL)+
  scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels)+
  theme(legend.position = c(0.85, 0.85)) -> x
ggsave("./research_projects/bbrkc_cpue/figures/core_vessel_retained_catch.png", plot = x,
       height = 3, width = 5, units = "in")  
  
# trim count pot data
pot_sum %>%
  # convert fishery code to season year
  f_sdr("fishery", type = "fishery_code") %>%
  dplyr::select(opening_year, adfg, biotwine_ok, date, longitude, latitude, statarea, e_w, depth, soak, gear, 
                mesh_size, female, sublegal, totallegal) %>%
  rename(season = opening_year,
         stat_area = statarea) %>%
  mutate(rationalized = ifelse(season >= 2005, T, F)) %>%
  # filter out bad biotwine and missing data
  filter(biotwine_ok != "N",
         !is.na(date), date != -9,
         !is.na(adfg), adfg != -9,
         !is.na(depth), depth != -9, depth > 20,
         !is.na(soak), soak != -9) %>%
  # extract month from data
  mutate(date = mdy(date),
         month = month(date),
  # add statarea region
         sa1 = as.numeric(substring(stat_area, 1, 3)),
         sa2 = as.numeric(substring(stat_area, 4, 6)),
         subregion = case_when((sa1 > 630 & sa2 > 600) ~ "NW",
                          (sa1 > 630 & sa2 <= 600) ~ "SW",
                          (sa1 < 630 & sa2 <= 601) ~ "SE",
                          (sa1 < 630 & sa2 > 600) ~ "NE")) %>%
  # remove hauls with questionable location
  left_join(stat_area %>%
              mutate(stat_area = as.numeric(stat_area)) %>%
              group_by(stat_area) %>%
              summarise(lon_min = min(long),
                        lon_max = max(long),
                        lat_min = min(lat),
                        lat_max = max(lat)),
            by = "stat_area") %>%
  
  mutate(locale_ok = ifelse((!is.na(longitude) & longitude != -9 & !is.na(latitude) & latitude != -9 & !is.na(stat_area) & stat_area != -9), 
                            (longitude >= lon_min & longitude <= lon_max & latitude >= lat_min & latitude <= lat_max),
                            NA)) %>%
  filter(locale_ok != F) %>%
  # remove columns that are not needed from data
  dplyr::select(-sa1, -sa2, -lon_min, -lon_max, -lat_min, -lat_max, -locale_ok) -> std_dat

# filter for core vessels
std_dat %>% 
  filter(adfg %in% core_vessels) -> std_dat_core


# eda ----

## nominal cpue 
std_dat %>%
  group_by(season) %>%
  summarise(cpue = mean(totallegal, na.rm = T),
            l95 = cpue + sqrt(var(totallegal, na.rm = T) / n()) * qnorm(0.025),
            u95 = cpue + sqrt(var(totallegal, na.rm = T) / n()) * qnorm(0.975)) %>%
  mutate(data = "All Obs Vessels") %>%
  bind_rows(std_dat_core %>%
              group_by(season) %>%
              summarise(cpue = mean(totallegal, na.rm = T),
                        l95 = cpue + sqrt(var(totallegal, na.rm = T) / n()) * qnorm(0.025),
                        u95 = cpue + sqrt(var(totallegal, na.rm = T) / n()) * qnorm(0.975)) %>%
              mutate(data = "Core Obs Vessels")) %>%
  ggplot()+
  geom_point(aes(x = season, y = cpue, color = data))+
  geom_line(aes(x = season, y = cpue, group = data, color = data))+
  geom_errorbar(aes(x = season, ymin = l95, ymax = u95, group = data, color = data), width = 0.3)+
  labs(x = NULL, y = "CPUE (legal crab per pot)", color = NULL)+
  scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels)+
  theme(legend.position = c(0.15, 0.85)) -> x
ggsave("./research_projects/bbrkc_cpue/figures/core_vessel_nominal_cpue.png", plot = x,
       height = 3, width = 5, units = "in") 

## depth
std_dat_core %>%
  ggplot()+
  geom_density(aes(x = depth))+
  labs(x = "Depth (fa)", y = "Density") -> p1
std_dat_core %>%
  ggplot()+
  geom_point(aes(x = depth, y = totallegal), alpha = 0.3)+
  geom_smooth(aes(x = depth, y = totallegal), method = "loess", se = F)+
  labs(x = "Depth (fa)", y = "CPUE (legal crab per pot)") -> p2
ggsave("./research_projects/bbrkc_cpue/figures/depth.png", 
       plot = cowplot::plot_grid(p1, p2, nrow = 1),
       height = 3, width = 6, units = "in") 

## soak time
std_dat_core %>%
  ggplot()+
  geom_density(aes(x = soak))+
  labs(x = "Soak Time (hr)", y = "Density") -> p1
std_dat_core %>%
  ggplot()+
  geom_point(aes(x = soak, y = totallegal), alpha = 0.3)+
  geom_smooth(aes(x = soak, y = totallegal), method = "loess", se = F)+
  labs(x = "Soak Time (hr)", y = "CPUE (legal crab per pot)") -> p2
ggsave("./research_projects/bbrkc_cpue/figures/soak.png", 
       plot = cowplot::plot_grid(p1, p2, nrow = 1),
       height = 3, width = 6, units = "in") 

## map of regions
f_base_map +
  geom_point(data = std_dat_core, aes(x = longitude, y = latitude), alpha = 0.3)+
  geom_polygon(data = stat_area %>%
                 filter(stat_area %in% std_dat_core$stat_area) %>%
                 mutate(stat_area = as.numeric(stat_area)) %>%
                 left_join(dplyr::select(std_dat_core, stat_area, subregion)),
               aes(x = long, y = lat, group = group, fill = subregion), color = 1, alpha = 0.75) +
  geom_hline(yintercept = 56.5, linetype = 2, size = 1) +
  geom_vline(xintercept = -163, linetype = 2, size = 1) +
  coord_quickmap(xlim = c(-166.5, -159.5), ylim = c(54.75, 58.25)) +
  theme(legend.position = "none") -> x
ggsave("./research_projects/bbrkc_cpue/figures/area_map.png", plot = x, height = 4, width = 5, units = "in")

## distance from center
# std_dat_core %>%
#   filter(longitude < -100, latitude > 50) %>%
#   group_by(season) %>%
#   summarise(mean_lon = mean(longitude, na.rm = T),
#             mean_lat = mean(latitude, na.rm = T)) -> centroids
# std_dat_core %>%
#   filter(longitude < -100, latitude > 50) %>%
#   left_join(centroids) %>%
#   rownames_to_column() %>%
#   group_by(rowname) %>%
#   mutate(dist_from_center = geosphere::distHaversine(p1 = c(longitude, latitude), p2 = c(mean_lon, mean_lat)) * 0.000539957) -> std_dat_core
# ### example of 2007 (year where this might not be a good metric)
# std_dat_core %>% 
#   filter(season == 2007) %>%
#   ggplot()+
#   geom_point(aes(x = longitude, y = latitude), color = "grey50", alpha = 0.1)+
#   geom_point(data = centroids %>% filter(season == 2007), aes(x = mean_lon, y = mean_lat), color = "blue", size = 2)+
#   labs(x = expression(paste(Longitude^o,~'W')), 
#        y = expression(paste(Latitude^o,~'N'))) -> p1
# std_dat_core %>% 
#   filter(season == 2007) %>%
#   ggplot()+
#   geom_point(aes(x = dist_from_center, y = totallegal), alpha = 0.1)+
#   geom_smooth(aes(x = dist_from_center, y = totallegal), method = "loess", se = F)+
#   labs(x = "Distance from Centroid (nmi)", y = "CPUE (legal crab per pot)") -> p2
# ### example of 2020 (year where this might not be a good metric)
# std_dat_core %>% 
#   filter(season == 2020) %>%
#   ggplot()+
#   geom_point(aes(x = longitude, y = latitude), color = "grey50", alpha = 0.1)+
#   geom_point(data = centroids %>% filter(season == 2020), aes(x = mean_lon, y = mean_lat), color = "blue", size = 2)+
#   labs(x = expression(paste(Longitude^o,~'W')), 
#        y = expression(paste(Latitude^o,~'N'))) -> p1.1
# std_dat_core %>% 
#   filter(season == 2000) %>%
#   ggplot()+
#   geom_point(aes(x = dist_from_center, y = totallegal), alpha = 0.1)+
#   geom_smooth(aes(x = dist_from_center, y = totallegal), method = "loess", se = F)+
#   labs(x = "Distance from Centroid (nmi)", y = "CPUE (legal crab per pot)") -> p2.1
# 

## month
std_dat_core %>%
  mutate(month = month.abb[month],
         month = factor(month, levels = c("Sep", "Oct", "Nov", "Dec", "Jan"))) %>%
  ggplot()+
  geom_boxplot(aes(x = month, y = totallegal))+
  labs(x = "Month", y = "CPUE (legal crab per pot)") -> x 
ggsave("./research_projects/bbrkc_cpue/figures/month.png", plot = x, height = 4, width = 5, units = "in")

## gear type
std_dat_core %>%
  filter(!(gear %in% 1:2)) %>%
  count(season, gear) %>%
  mutate(gear = case_when(gear == 6 ~ "6' x 6'",
                          gear == 7 ~ "7' x 7'",
                          gear == 8 ~ "8' x 8'",
                          gear == 9 ~ "5.5' x 5.5'",
                          gear == 10 ~ "6.5' x 6.5'",
                          gear == 11 ~ "7.5' x 7.5'",
                          gear == 20 ~ "7' x 8'")) %>%
  ggplot()+
  geom_bar(aes(x = season, y = n, fill = factor(gear)), color = 1, width = 1, position = "stack", stat = "identity")+
  scale_x_continuous(breaks = yr_axis$breaks, labels = yr_axis$labels)+
  labs(x = NULL, y = "Pots Sampled", fill = NULL)+
  theme(legend.position = c(0.1, 0.75)) -> p1
std_dat_core %>%
  filter(!(gear %in% 1:2)) %>%
  mutate(gear = case_when(gear == 6 ~ "6' x 6'",
                          gear == 7 ~ "7' x 7'",
                          gear == 8 ~ "8' x 8'",
                          gear == 9 ~ "5.5' x 5.5'",
                          gear == 10 ~ "6.5' x 6.5'",
                          gear == 11 ~ "7.5' x 7.5'",
                          gear == 20 ~ "7' x 8'")) %>%

  ggplot()+
  geom_boxplot(aes(x = gear, y = totallegal))+
  labs(x = "Pot Size", y = "CPUE (legal crab per pot)") -> p2
ggsave("./research_projects/bbrkc_cpue/figures/pot_size.png", 
       plot = cowplot::plot_grid(p1, p2, nrow = 2),
       height = 6, width = 6, units = "in") 



