library(tidyverse)
library(sf)

# Get the datasets
tr <- read_rds("data/tracts_with_distance.rds")
ent <- read_rds("data/diversity_by_year.rds")

# Organize the comparative entropy scores by year along with distances
diversity <- map(tr, ~{
  dist_tbl <- .x %>%
    select(tract_id, distmiles) %>%
    st_drop_geometry()
  
  inner_join(ent, dist_tbl, by = "tract_id")
})

qs::qsave(diversity, "data/metro_diversity_by_year.qs")

# Attach the 2020 diversity and race values to the tracts
diversity2020 <- filter(ent, year == "2020")
race_ethnicity <- read_rds("data/race_data_prepped.rds") %>%
  filter(group != "other",
         year == 2020) %>%
  mutate(pop = as.integer(pop)) %>%
  pivot_wider(id_cols = tract_id,
              names_from = group,
              values_from = pop)

tract <- map(tr, ~{
  left_join(.x, diversity2020, by = "tract_id") %>%
    select(-year, -distance, -metro_id) %>%
    st_transform(4326) %>%
    left_join(race_ethnicity, by = "tract_id")
})

qs::qsave(tract, "data/tracts_with_distance_2020.qs")



