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
  library(glue)
  library(highcharter)
  library(htmltools)
  library(monochromeR)
  library(numform)
  library(packcircles)
  library(plotly)
  library(prismatic)
  library(RColorBrewer)
  library(scales)
  library(sf)
  library(shiny)
  library(sunburstR)
  library(tidyverse)
  library(treemap)
}

##### Load Data #####

# occurrence data summarised
load("data/occ_summary120.rds")
load("data/regions.rds")
load("data/taxa_colours.rds")

##### Visualisations #####
###### MAP ######
# group data
map_data <- occ_summary |>
  group_by(IBRAIMCRA_region) |>
  summarise(count = sum(count), .groups = "drop") |>
  right_join(regions, by = c("IBRAIMCRA_region" = "region_name")) |>
  st_as_sf(crs = st_crs(regions))

####### Interactivity #######
int_map <- ggplot() +
  geom_sf_interactive(data = map_data |> filter(IBRA_IMCRA == "IMCRA"),
                      aes(fill = count,
                          tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                      colour = "gray30") +
  scale_fill_distiller(name = "IMCRA",
                       type = "seq",
                       palette = "PuBu",
                       na.value = "grey97",
                       direction = 1,
                       trans = "sqrt",
                       breaks = c(0, 2000, 8000, 18000, 32000, 50000),
                       #labels = c(1, 50, 500, 5000, 50000),
                       #labels = c("0.001", "0.01", "0.1", "1", "10"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "top")) +
  # adds new colour scale
  ggnewscale::new_scale_fill() +
  geom_sf_interactive(data = map_data |> filter(IBRA_IMCRA == "IBRA"),
                      aes(fill = count,
                          tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                      colour = "gray10") +
  scale_fill_distiller(name = "IBRA",
                       type = "seq",
                       palette = "YlOrBr",
                       na.value = "grey97",
                       direction = 1,
                       trans = "sqrt",
                       breaks = c(0, 40000, 160000, 360000, 640000, 1000000),
                       #labels = c("0.1", "1", "10", "100"),
                       guide = guide_colorsteps(direction = "horizontal",
                                                label.position = "bottom",
                                                title.position = "top")) +
  coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
  theme_void() +
  theme(legend.box = "vertical",
        legend.position = c(0.3, 0.175),
        legend.key.width = unit(12, "mm"),
        legend.key.height = unit(3, "mm"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))
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

###### Hierarchical Taxa data ######
tree_data <- occ_summary |>
  group_by(kingdom, phylum, class, order) |>
  summarise(count = sum(count), .groups = "drop") |>
  replace_na(list(kingdom = "[UNIDENTIFIED]",
                  phylum = "[UNIDENTIFIED]",
                  class = "[UNIDENTIFIED]",
                  order = "[UNIDENTIFIED]"))

####### PLOTLY SUNBURST MAP #######
# create data in plotly form
sunburst_data <- rbind(
  tree_data |>
    mutate(id = paste(kingdom, phylum, class, order, " "),
           parent = paste(kingdom, phylum, class, order),
           names = "",
           color = "#FFFFFF") |>
    select(id, parent, names, count, color),
  tree_data |>
    mutate(id = paste(kingdom, phylum, class, order),
           parent = paste(kingdom, phylum, class)) |>
    left_join(taxa_colours |> select(-level), by = c("kingdom", "phylum", "class", "order")) |>
    rename(names = order) |>
    select(id, parent, names, count, color),
  tree_data |>
    group_by(kingdom, phylum, class) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(id = paste(kingdom, phylum, class),
           parent = paste(kingdom, phylum),
           order = NA) |>
    left_join(taxa_colours |> select(-level), by = c("kingdom", "phylum", "class", "order")) |>
    rename(names = class) |>
    select(id, parent, names, count, color),
  tree_data |>
    group_by(kingdom, phylum) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(id = paste(kingdom, phylum),
           parent = kingdom,
           class = NA, order = NA) |>
    left_join(taxa_colours |> select(-level), by = c("kingdom", "phylum", "class", "order")) |>
    rename(names = phylum) |>
    select(id, parent, names, count, color),
  tree_data |>
    group_by(kingdom) |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(id = paste(kingdom),
           parent = "TOTAL",
           phylum = NA, class = NA, order = NA) |>
    left_join(taxa_colours |> select(-level), by = c("kingdom", "phylum", "class", "order")) |>
    rename(names = kingdom) |>
    select(id, parent, names, count, color),
  tree_data |>
    summarise(count = sum(count), .groups = "drop") |>
    mutate(id = "TOTAL", parent = "", names = "TOTAL", color = "#FFFFFF") |>
    select(id, parent, names, count, color)
) |>
  mutate(color = if_else(names == "[UNIDENTIFIED]", "#E5E5E5", color)) |>
  distinct() |>
  mutate(names = ifelse((grepl("\\[UNIDENTIFIED\\]$", parent) & names %in% c("", "[UNIDENTIFIED]")),
                        "", names),
         color = ifelse((grepl("\\[UNIDENTIFIED\\]$", parent) & names %in% c("", "[UNIDENTIFIED]")),
                        "#FFFFFF", color),
         count = ifelse((grepl("\\[UNIDENTIFIED\\]$", parent) & names %in% c("", "[UNIDENTIFIED]")),
                        0, count)) |>
  arrange(desc(id)) |>
  mutate(hovertemplate = ifelse(names == "",
                                NA,
                                glue("{names}<br>{f_comma(count)}<extra></extra>")))

sunburst_plotly <- plot_ly(
  data = sunburst_data,
  type = "sunburst",
  ids = ~id,
  labels = ~names,
  parents = ~parent,
  values = ~count,
  branchvalues = "total",
  maxdepth = 2,
  insidetextorientation = "radial",
  sort = FALSE,
  rotation = 90,
  hovertemplate = ~hovertemplate,
  hoverlabel = list(align = "left"),
  marker = list(colors = ~color)
) |>
  layout(colorway = ~color)

###### d2b SUNBURST ######
sunburstR_data1 <- tree_data |>
  mutate(categories = paste(kingdom, phylum, class, order, sep = "-")) |>
  select(categories, count) |>
  as.data.frame()

sunburstR_data <- (occ_summary |>
    group_by(kingdom, phylum, class, order) |>
    summarise(count = sum(count), .groups = "drop") |>
    treemap(
      index = c("kingdom", "phylum", "class", "order"),
      vSize = "count",
      draw = FALSE,
      palette = "Dark2"
    ))$tm |>
  arrange(kingdom, phylum, class, order)

sunburstR_data <- sunburstR_data |>
  select(-color) |>
  left_join(taxa_colours, by = c("kingdom", "phylum", "class", "order", "level")) |>
  d3_nest(value_cols = c("vSize", "vColor", "stdErr", "vColorValue", "level", "x0","y0", "w", "h", "color"))

sund2b(
  sunburstR_data,
  colors = sunburstR_data1$color,
  # colors = htmlwidgets::JS(
  #   "function(name, d){return d.color || '#ccc';}"
  # ),
  valueField = "vSize",
  showLabels = TRUE,
  rootLabel = "RESET",
  breadcrumbs = sund2bBreadcrumb(enabled = FALSE),
)
##### Yearly Diverging Barplot #####
barplot_data <- occ_summary |>
  group_by(year) |>
  summarise(count = sum(count), .groups = "drop")

###### Prep data for divergence ######
div_barplot_data <- occ_summary |>
  group_by(year, cs) |>
  summarise(count = sum(count), .groups = "drop") |>
  mutate(count = ifelse(cs == "Non-citizen Science", -1 * count, count))

ggplot(div_barplot_data) +
  geom_bar(aes(x = year, y = count, fill = cs), 
           stat = "identity", position = "identity") +
  scale_y_continuous(breaks = pretty(div_barplot_data$count),
                     labels = label_number(
                       accuracy = 0.1, 
                       scale_cut = cut_short_scale())(abs(pretty(div_barplot_data$count))) %>%
                       {gsub("\\.0", "", .)}) +
  scale_x_continuous(limits = c(min(div_barplot_data$year) - 1, 
                                max(div_barplot_data$year) + 1),
                     breaks = pretty(div_barplot_data$year, 
                                     n = diff(range(div_barplot_data$year)) / 10)) +
  scale_fill_manual(values = c("#bebada", "#fccde5")) +
  guides(fill = guide_legend(title = "Data Resource Type")) +
  xlab("Year") +
  ylab("Number of Occurrences") +
  theme_classic()

ggplot(barplot_data) +
  geom_bar(aes(x = year, y = count), fill = "#bebada", 
           stat = "identity", position = "identity") +
  scale_y_continuous(breaks = pretty(c(0, barplot_data$count)),
                     labels = label_number(
                       accuracy = 0.1, 
                       scale_cut = cut_short_scale())(pretty(c(0, barplot_data$count))) %>%
                       {gsub("\\.0", "", .)},
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(min(barplot_data$year) - 1, 
                                max(barplot_data$year) + 1),
                     breaks = pretty(barplot_data$year, 
                                     n = diff(range(barplot_data$year)) / 10)) +
  xlab("Year") +
  ylab("Number of Occurrences") +
  theme_classic()

###### Interactive Plots ######
int_barplot <- ggplot(barplot_data) +
  geom_bar_interactive(aes(x = year, y = count,
                               tooltip = year, data_id = year),
                           fill = "#bebada", 
                           alpha = 0.9,
                           stat = "identity", position = "identity") +
      scale_y_continuous(breaks = pretty(c(0, barplot_data$count)),
                         labels = label_number(
                           accuracy = 0.1, 
                           scale_cut = cut_short_scale())(pretty(c(0, barplot_data$count))) %>%
                           {gsub("\\.0", "", .)},
                         expand = c(0, 0)) +
      scale_x_continuous(limits = c(min(barplot_data$year) - 1, 
                                    max(barplot_data$year) + 1),
                         breaks = pretty(barplot_data$year, 
                                         n = diff(range(barplot_data$year)) / 10)) +
      xlab("Year") +
      ylab("Number of Occurrences") +
      theme_classic()
girafe(ggobj = int_barplot,
       options = list(
         opts_selection(css = "stroke-width:0.3;",
                        type = "multiple",
                        only_shiny = FALSE),
         opts_selection_inv(css = "opacity:0.3;"),
         opts_hover(css = "stroke-width:1;stroke:black"),
         opts_toolbar(position = "top"),
         opts_sizing(rescale = TRUE)
       ))

# Diverging interactivity
int_div_barplot <- ggplot(div_barplot_data) +
  geom_bar_interactive(aes(x = year, y = count, fill = cs,
                           tooltip = year, data_id = year),
                       alpha = 0.9,
                       stat = "identity", position = "identity") +
  scale_y_continuous(breaks = pretty(c(0, div_barplot_data$count)),
                     labels = label_number(
                       accuracy = 0.1, 
                       scale_cut = cut_short_scale())(pretty(c(0, div_barplot_data$count))) %>%
                       {gsub("\\.0", "", .)}) +
  scale_x_continuous(limits = c(min(div_barplot_data$year) - 1, 
                                max(div_barplot_data$year) + 1),
                     breaks = pretty(div_barplot_data$year, 
                                     n = diff(range(div_barplot_data$year)) / 10)) +
  scale_fill_manual_interactive(name = "Data Resource",
                                values = c("#bebada", "#fccde5"), 
                                data_id = c("Citizen Science", "Non-citizen Science"),
                                tooltip = c("Citizen Science", "Non-citizen Science")) +
  xlab("Year") +
  ylab("Number of Occurrences") +
  theme_classic()
girafe(ggobj = int_div_barplot,
       options = list(
         opts_selection(css = "stroke-width:0.3;",
                        type = "multiple",
                        only_shiny = FALSE),
         opts_selection_inv(css = "opacity:0.3;"),
         opts_hover(css = "stroke-width:1;stroke:black"),
         opts_toolbar(position = "top"),
         opts_sizing(rescale = TRUE)
       ))


##### Circular Packing plot #####
circular_data <- occ_summary |>
  group_by(basisOfRecord) |>
  summarise(count = sum(count), .groups = "drop") |>
  mutate(basisOfRecord_text = gsub(" ", "\n", basisOfRecord)) |>
  arrange(desc(count)) |>
  mutate(fill = generate_palette("#817E94", "go_lighter", n_colours = 10)[(10 - n() + 1):10],
         colour = ifelse(clr_extract_luminance(fill) > 50, "black", "white"),
         count_label = prettyNum(count, big.mark = ","))

circular_text_data <- circular_data |>
  cbind(circleProgressiveLayout(circular_data$count, sizetype = "area"))

circular_plot_data <- circleLayoutVertices(
  circular_text_data, npoints = 100, xysizecols = 7:9, idcol = 3
) |>
  left_join(circular_text_data |> select(basisOfRecord_text, count_label),
            by = c("id" = "basisOfRecord_text"))

int_circular_plot <- ggplot() + 
  geom_polygon_interactive(data = circular_plot_data,
                           aes(x = x, y = y, group = id, fill = id, 
                               tooltip = sprintf("%s\n %s", id, count_label), 
                               data_id = id), 
                           colour = "white", alpha = 1) +
  geom_text(data = circular_text_data,
            aes(x = x, y = y, label = basisOfRecord_text, 
                size = radius, colour = basisOfRecord_text)) +
  scale_fill_manual(values = circular_text_data$fill) +
  scale_colour_manual(values = circular_text_data$colour) +
  scale_size(range = c(0,5)) +
  theme_void() + 
  theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
  coord_equal()
girafe(ggobj = int_circular_plot,
       options = list(
         opts_selection(css = "stroke-width:0.3;",
                        type = "multiple",
                        only_shiny = FALSE),
         opts_selection_inv(css = "opacity:0.3;"),
         opts_hover(css = "stroke-width:1;stroke:black"),
         opts_toolbar(position = "top"),
         opts_zoom(max = 5),
         opts_sizing(rescale = TRUE)
       ))
