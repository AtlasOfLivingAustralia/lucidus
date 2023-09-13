##### Description #####
# The function of this script is to clean the initial dataset of 2001-05 
# records into a tibble that can be used with shiny to produce the intended
# visualisations.

##### Libraries #####
{
  library(arrow)
  library(janitor)
  library(sf)
  library(stringr)
  library(tidyverse)
  library(tools)
  library(treemap)
}

##### Occurrence Data #####
###### Load Data ######

# Occurrence Data
occ_data <- map(
  .x = list.files("raw-data/occ_files/", full.names = TRUE),
  .f = read_parquet) |>
  list_rbind()

# citizen-science classifications
cs_datasets <- read_csv("raw-data/ALA-CS-datasets.csv", show_col_types = FALSE)

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
  mutate(cs = replace_na(cs, "Non-citizen Science")) |>
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

save(occ_summary, file = "data/occ_summary.rds")


##### 1900-2020 Data #####
occ_summary <- read_csv("raw-data/occ_summary120.csv") |>
  mutate(basisOfRecord = toTitleCase(tolower(gsub("_", " ", basisOfRecord))))
save(occ_summary, file = "data/occ_summary120.rds")

##### Regions Data #####
###### Load + Process Data ######
aus_outline <- st_read("raw-data/shapefiles/IBRA7_regions/ibra7_regions.shp") |>
  st_make_valid() |>
  st_union() |>
  st_make_valid() |>
  st_simplify(preserveTopology = TRUE, dTolerance = 1000) |>
  st_as_sf()
save(aus_outline, file = "data/aus_outline.rds")  

IBRA_regions <- st_read("raw-data/shapefiles/IBRA7_regions/ibra7_regions.shp") |>
  st_make_valid() |>
  st_simplify(preserveTopology = TRUE, dTolerance = 1000) |>
  select(REG_CODE_7, REG_NAME_7) |>
  rename(region_code = REG_CODE_7, region_name = REG_NAME_7) |>
  mutate(IBRA_IMCRA = "IBRA")
IMCRA_mesoscale <- st_read("raw-data/shapefiles/IMCRA_mesoscale/imcra4_meso.shp") |>
  st_make_valid() |>
  st_simplify(preserveTopology = TRUE, dTolerance = 1000) |>
  select(MESO_ABBR, MESO_NAME) |>
  rename(region_code = MESO_ABBR, region_name = MESO_NAME) |>
  mutate(IBRA_IMCRA = "IMCRA")

regions <- rbind(IBRA_regions, IMCRA_mesoscale) |>
  st_as_sf(crs = st_crs(IBRA_regions))

save(regions, file = "data/regions.rds")  

##### Taxa Colour Data #####

tree_palette <- 
  c(
    "#CC6677", # Animalia
    "#882255", # Bacteria
    "#AA4499", # Chromista
    "#999933", # Eukaryota
    "#DDCC77", # Fungi
    "#117733", # Plantae
    "#332288", # Protista
    "#999933"  # Protozoa
)

treemap_data <- (occ_summary |>
                   group_by(kingdom, phylum, class, order) |>
                   summarise(count = sum(count), .groups = "drop") |>
                   treemap(
                     index = c("kingdom", "phylum", "class", "order"),
                     vSize = "count",
                     draw = FALSE,
                     palette = tree_palette
                   ))$tm |>
  arrange(kingdom, phylum, class, order)

taxa_colours <- treemap_data |> 
  select(kingdom, phylum, class, order, level, color)

save(taxa_colours, file = "data/taxa_colours.rds")
