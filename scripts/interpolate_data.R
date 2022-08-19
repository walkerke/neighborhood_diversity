# interpolate_data.R
#
# Goal here: adjust race / ethnicity data from 2000 and 2010 to 2020 boundaries
# 
# First, get weights for states you'll need
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)
sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)


tract_shapes <- read_rds("data/tracts_with_distance.rds")

# state_fips <- map(tract_shapes, ~{
#   unique(str_sub(.x$tract_id, 1, 2))
# }) %>%
#   unlist() %>%
#   unique()

# Cache the blocks for each year
# walk(state_fips, ~{
#   blocks(state = .x, year = 2000)
#   blocks(state = .x, year = 2010)
# })

# Given that we are doing race / ethnicity, we can "roll up" block centroids
# to 2020 Census tracts
sf1_00 <- load_variables(2000, "sf1")

# Variables we need for 2000 include: 
# - P004002: Hispanic
# - P004005: White
# - P004006: Black
# - P004008: Asian
race00 <- map(tract_shapes, function(x) {
  print(glue::glue("Working on {unique(x$metro_id)}..."))
  
  # Retain only the GEOID column
  metro_tracts <- select(x, tract_id)
  
  # Find the state / county combos you need
  state_codes <- unique(str_sub(metro_tracts$tract_id, 1, 2))
  
  block_centroids <- map_dfr(state_codes, function(y) {
    county_codes <- metro_tracts %>%
      filter(str_sub(tract_id, 1, 2) == y) %>%
      pull(tract_id) %>%
      str_sub(3, 5) %>%
      unique()
    
    block_data <- get_decennial(
      geography = "block",
      variables = c(
        Hispanic = "P004002",
        White = "P004005",
        Black = "P004006",
        Asian = "P004008"
      ), 
      year = 2000,
      state = y,
      county = county_codes,
      geometry = TRUE,
      output = "wide"
    ) %>%
      rename(block_id = GEOID) %>%
      st_transform(st_crs(metro_tracts)) %>%
      st_centroid()
    
    to_2020 <- metro_tracts %>%
      st_join(block_data) %>%
      st_drop_geometry() %>%
      group_by(tract_id) %>%
      summarize(across(Hispanic:Asian,
                       .fns = ~sum(.x, na.rm = TRUE)))
    
    to_2020
  })
  
  block_centroids
})

# Variables we need for 2010 include: 
# - : Hispanic
# - : White
# - : Black
# - : Asian
race10 <- map(tract_shapes, function(x) {
  print(glue::glue("Working on {unique(x$metro_id)}..."))
  
  # Retain only the GEOID column
  metro_tracts <- select(x, tract_id)
  
  # Find the state / county combos you need
  state_codes <- unique(str_sub(metro_tracts$tract_id, 1, 2))
  
  block_centroids <- map_dfr(state_codes, function(y) {
    county_codes <- metro_tracts %>%
      filter(str_sub(tract_id, 1, 2) == y) %>%
      pull(tract_id) %>%
      str_sub(3, 5) %>%
      unique()
    
    block_data <- get_decennial(
      geography = "block",
      variables = c(
        Hispanic = "",
        White = "",
        Black = "",
        Asian = ""
      ), 
      year = 2010,
      state = y,
      county = county_codes,
      geometry = TRUE,
      output = "wide"
    ) %>%
      rename(block_id = GEOID) %>%
      st_transform(st_crs(metro_tracts)) %>%
      st_centroid()
    
    to_2020 <- metro_tracts %>%
      st_join(block_data) %>%
      st_drop_geometry() %>%
      group_by(tract_id) %>%
      summarize(across(Hispanic:Asian,
                       .fns = ~sum(.x, na.rm = TRUE)))
    
    to_2020
  })
  
  block_centroids
})
