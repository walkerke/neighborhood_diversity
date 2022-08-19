# get_tracts_and_distances.R

#' Steps:
#' 1. Grab all US tracts and transform to an equidistant CRS
#' 2. Find the top 50 metros by population 
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

city_halls <- read_csv("data/city_halls.csv")

us_tracts <- tracts(cb = TRUE, year = 2019) %>%
  st_transform('ESRI:102005')

metros <- core_based_statistical_areas(cb = TRUE, year = 2019) %>%
  select(metro_id = GEOID) %>%
  filter(metro_id %in% as.character(city_halls$metro_id)) %>%
  st_transform('ESRI:102005')

metro_tracts <- us_tracts %>%
  select(tract_id = GEOID) %>%
  st_join(metros, join = st_within, left = FALSE)

metro_ids <- unique(metros$metro_id)
names(metro_ids) <- metro_ids

# Iterate through the metros and calculate distances
tracts_with_distance <- map(metro_ids, ~{
  halls <- filter(city_halls, metro_id == .x) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    st_transform(st_crs(metros))
  
  local_tracts <- filter(metro_tracts, metro_id == .x)
  
  tract_distances <- local_tracts %>%
    st_distance(halls, by_element = FALSE) %>%
    apply(1, min)
  
  local_tracts$distance <- tract_distances
  local_tracts$distmiles <- local_tracts$distance * 0.000621371
  
  local_tracts
})

write_rds(tracts_with_distance, "data/tracts_with_distance.rds")