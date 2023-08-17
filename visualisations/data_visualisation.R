##### Description #####
# The function of this script is to provide initial visualisations of the 
# summarised dataset in preparation for including them in a shiny app.

##### Libraries #####
{
  library(crosstalk)
  library(d3r)
  library(d3treeR) #devtools::install_github("timelyportfolio/d3treeR")
  library(galah)
  library(ggiraph)
  library(ggnewscale)
  library(ggthemes)
  library(highcharter)
  library(htmltools)
  library(plotly)
  library(RColorBrewer)
  library(sf)
  library(shiny)
  library(sunburstR)
  library(tidyverse)
  library(treemap)
}

##### Load Data #####

# occurrence data summarised
load("data/occ_summary.RData")

# Location Data
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

##### Visualisations #####
###### MAP ######
# group data
map_data <- occ_summary |>
  group_by(IBRAIMCRA_region) |>
  summarise(count = sum(count), .groups = "drop") |>
  right_join(regions, by = c("IBRAIMCRA_region" = "region_name")) |>
  st_as_sf(crs = st_crs(regions))

ggplot() + 
  geom_sf(data = map_data |> filter(IBRA_IMCRA == "IMCRA"),
          aes(fill = count),
          colour = "gray30") +
  scale_fill_distiller(name = "IMCRA",
                       type = "seq",
                       palette = "BuPu",
                       direction = 1,
                       trans = "sqrt",
                       #labels = c(1, 50, 500, 5000, 50000),
                       #labels = c("0.001", "0.01", "0.1", "1", "10"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "left")) +
  # adds new colour scale
  ggnewscale::new_scale_fill() +
  geom_sf(data = map_data |> filter(IBRA_IMCRA == "IBRA"),
          aes(fill = count),
          colour = "gray10") +
  scale_fill_distiller(name = "IBRA",
                       type = "seq",
                       palette = "YlOrBr",
                       direction = 1,
                       trans = "sqrt",
                       #labels = c("0.1", "1", "10", "100"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "left")) +
  #coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  theme_void() +
  theme(legend.position = "none")

####### Interactivity #######
int_map <- ggplot() +
  geom_sf_interactive(data = map_data |> filter(IBRA_IMCRA == "IMCRA"),
                      aes(fill = count,
                          tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                      colour = "gray30") +
  scale_fill_distiller(name = "IMCRA",
                       type = "seq",
                       palette = "PuBu",
                       direction = 1,
                       trans = "sqrt",
                       #labels = c(1, 50, 500, 5000, 50000),
                       #labels = c("0.001", "0.01", "0.1", "1", "10"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "left")) +
  # adds new colour scale
  ggnewscale::new_scale_fill() +
  geom_sf_interactive(data = map_data |> filter(IBRA_IMCRA == "IBRA"),
                      aes(fill = count,
                          tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                      colour = "gray10") +
  scale_fill_distiller(name = "IBRA",
                       type = "seq",
                       palette = "YlOrBr",
                       direction = 1,
                       trans = "sqrt",
                       #labels = c("0.1", "1", "10", "100"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "left")) +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  theme_void() +
  theme(legend.position = "none")
girafe(ggobj = int_map,
       options = list(
         opts_selection(css = "stroke-width:0.3;", 
                        type = "multiple", 
                        only_shiny = FALSE),
         opts_selection_inv(css = "opacity:0.3;"),
         opts_hover(css = "stroke-width:1;stroke:black"),
         opts_zoom(max = 5),
         opts_toolbar(position = "bottom")
         #opts_hover_inv(css = "opacity:0.3;")
       ))

###### TREEMAP ######
tree_data <-  occ_summary |>
  group_by(kingdom, phylum, class, order) |>
  summarise(count = sum(count), .groups = "drop") |>
  replace_na(list(kingdom = "[UNIDENTIFIED]", 
                  phylum = "[UNIDENTIFIED]", 
                  class = "[UNIDENTIFIED]", 
                  order = "[UNIDENTIFIED]")
  )

taxonomy_tree <- treemap(
  tree_data,
  index = c("kingdom", "phylum", "class", "order"),
  vSize = "count",
  type = "index",
  title = "ALA Taxonomy",
  fontsize.labels = 15
)

d3tree3(taxonomy_tree,
        rootname = "Taxonomy",
        width = "100%")

####### PLOTLY SUNBURST MAP #######
# create data in plotly form
sunburst_data <- rbind(
  tree_data |>
    mutate(id = paste(kingdom, phylum, class, order, " "),
           parent = paste(kingdom, phylum, class, order),
           names = "") |>
    select(id, parent, names, count),
  tree_data |>
    mutate(id = paste(kingdom, phylum, class, order),
           parent = paste(kingdom, phylum, class)) |>
    rename(names = order) |>
    select(id, parent, names, count),
  tree_data |>
    group_by(kingdom, phylum, class) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(id = paste(kingdom, phylum, class),
           parent = paste(kingdom, phylum)) |>
    rename(names = class) |>
    select(id, parent, names, count),
  tree_data |>
    group_by(kingdom, phylum) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(id = paste(kingdom, phylum),
           parent = kingdom) |>
    rename(names = phylum) |>
    select(id, parent, names, count),
  tree_data |>
    group_by(kingdom) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(id = paste(kingdom),
           parent = "") |>
    rename(names = kingdom) |>
    select(id, parent, names, count)
  )

sunburst_plotly <- plot_ly(
  data = sunburst_data,
  type = "sunburst",
  ids = ~id,
  labels = ~names,
  parents = ~parent,
  values = ~count,
  branchvalues = "total",
  maxdepth = 4
)

###### d2b SUNBURST ######
sunburstR_data <- tree_data |>
  mutate(categories = paste(kingdom, phylum, class, order, sep = "-")) |>
  select(categories, count) |>
  as.data.frame()

sund2b(sunburstR_data,
       valueField = "count",
       showLabels = TRUE,
       rootLabel = "RESET",
       colors = htmlwidgets::JS(
         "function(name, d){return d.color || '#ccc';}"
       ))
