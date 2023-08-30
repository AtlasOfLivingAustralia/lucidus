library(arrow)
library(janitor)
library(tidyverse)

# citizen-science classifications
cs_datasets <- read_csv("raw-data/ALA-CS-datasets.csv", show_col_types = FALSE)

cs_datasets <- cs_datasets %>%
  clean_names(.) %>%
  filter(!is.na(data_resource_name),
         !is.na(citizen_science))

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
  select(dataResourceName = data_resource_name, cs) %>%
  distinct()  

rm(cs_datasets)

# occ data
dir.create("raw-data/tmp")
dir.create("raw-data/tmp-agg")

ds <- open_dataset("raw-data/occ_files", format = "parquet")

ds |> 
  select(dataResourceName, 
         basisOfRecord,
         cl1048, 
         cl966,
         year,
         kingdom, 
         phylum,
         class, 
         order) |> 
  left_join(cs_categories, by = join_by(dataResourceName)) |> 
  group_by(year) |> 
  write_dataset(path = "raw-data/tmp", format = "parquet")

pq_files <- list.files("raw-data/tmp", full.names = TRUE, recursive = TRUE)

summarise_occ <- function(fpath) {
  
  year_id <- sub('.*=(.*?)/.*', '\\1', fpath)
  
  fpath |>
    read_parquet() |>
    mutate(cs = replace_na(cs, "Non-citizen Science")) |>
    pivot_longer(cols = c("cl1048", "cl966"),
                 names_to = "IBRAIMCRA_field",
                 values_to = "IBRAIMCRA_region",
                 values_drop_na = TRUE) |> 
    mutate(IBRAIMCRA_region = str_replace(string = IBRAIMCRA_region, 
                                          pattern = "Pilbarra",
                                          replacement = "Pilbara")) |> 
    group_by(IBRAIMCRA_region, cs, kingdom, phylum, class, order, basisOfRecord) |>
    summarise(count = n(), .groups = "drop") |> 
    mutate(year = year_id) |> 
    write_parquet(sink = paste0("raw-data/tmp-agg/year_", year_id, ".parquet"))
}

walk(pq_files, summarise_occ)

open_dataset("raw-data/tmp-agg", format = "parquet") |> 
  write_csv_arrow("raw-data/summarised_occ.csv")

open_dataset("raw-data/tmp-agg", format = "parquet") |> 
  collect() |> 
  write_rds(file = "raw-data/summarised_occ.rds")

unlink("raw-data/tmp", recursive = TRUE)
unlink("raw-data/tmp-agg", recursive = TRUE)
