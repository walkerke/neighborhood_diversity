library(tidyverse)
library(segregation)

nhgis <- read_csv("data/nhgis0104_csv/nhgis0104_csv/nhgis0104_ts_geog2010_tract.csv")

prepped <- nhgis %>%
  mutate(tract_id = paste0(STATEA, COUNTYA, TRACTA)) %>%
  select(tract_id, starts_with("CW7")) %>%
  pivot_longer(cols = starts_with("CW7"), 
               names_to = "variable", values_to = "value") %>%
  filter(str_sub(variable, start = -1L) == 0) %>%
  separate(variable, into = c("nhgis", "year"), 
           sep = 5) %>%
  mutate(group = case_when(
    nhgis == "CW7AA" ~ "white",
    nhgis == "CW7AB" ~ "black",
    nhgis == "CW7AC" ~ "aian",
    nhgis == "CW7AD" ~ "asian",
    nhgis == "CW7AE" ~ "other",
    nhgis == "CW7AF" ~ "two_or_more",
    TRUE ~ "hispanic"
  )) %>%
  group_by(tract_id, year, group) %>%
  summarize(pop = sum(value, na.rm = TRUE))

diversity_by_year <- prepped %>%
  filter(group != "other") %>%
  group_by(tract_id, year) %>%
  group_modify(~tibble(entropy = entropy(
    data = .x,
    group = "group",
    weight = "pop",
    base = 6
  )))

# Write out the dataset
write_rds(diversity_by_year, "data/diversity_by_year.rds")

# Write out `prepped` for future processing
write_rds(prepped, "data/race_data_prepped.rds")