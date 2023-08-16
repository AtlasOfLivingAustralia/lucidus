##### Description #####
# The function of this script is to clean the initial dataset of 2001-05 
# records into a tibble that can be used with shiny to produce the intended
# visualisations.

##### Libraries #####
{
  library(arrow)
  library(janitor)
  library(stringr)
  library(tidyverse)
}

##### Load Data #####

# Occurrence Data
occ_data <- map(
  .x = list.files("raw-data/occ_files/", full.names = TRUE),
  .f = read_parquet) |>
  list_rbind()

# citizen-science classifications
cs_datasets <- read_csv("raw-data/ALA-CS-datasets.csv")

cs_datasets <- cs_datasets %>%
  clean_names(.) %>%
  filter(!is.na(data_resource_name)) %>%
  filter(!is.na(citizen_science))

# Group categories of citizen science projects
cs_categories <- cs_datasets %>%
  select(data_resource_name, citizen_science, non_cs, data_biocache) %>%
  mutate(
    cs = case_when(
      citizen_science == "N" & non_cs == "N" ~ "Non-citizen Science",
      citizen_science == "Y" & non_cs == "Y" ~ "Citizen Science",
      citizen_science == "Y" & non_cs == "N" ~ "Citizen Science",
      citizen_science == "N" & non_cs == "Y" ~ "Non-citizen Science",
      TRUE ~ "Unidentified")) %>%
  rename(dataResourceName = data_resource_name) %>%
  select(dataResourceName, cs) %>%
  distinct()

rm(cs_datasets)

##### Process Data #####
# TO DO:
#   Join (and duplicate IBRA/IMCRA)
#   Join Citizen/Non-CS

occ_data <- occ_data |> 
  left_join(
    cs_categories,
    by = "dataResourceName"
  ) |>
  pivot_longer(
    cols = c("cl1048", "cl966"),
    names_to = "IBRAIMCRA_field",
    values_to = "IBRAIMCRA_region",
    values_drop_na = TRUE
  ) |> 
  mutate(IBRAIMCRA_region = str_replace(string = IBRAIMCRA_region, 
                                        pattern = "Pilbarra",
                                        replacement = "Pilbara"))

occ_summary <- occ_data |>
  group_by(year, IBRAIMCRA_region, cs, kingdom, phylum, class, order) |>
  summarise(count = n(), .groups = "drop")

save(occ_summary, file = "data/occ_summary.RData")  
