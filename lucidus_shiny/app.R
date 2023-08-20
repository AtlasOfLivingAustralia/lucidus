#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Libraries #####

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
library(scales)
library(sf)
library(shiny)
library(sunburstR)
library(tidyverse)
library(treemap)

##### Load Data #####
load("../data/occ_summary.RData")
load("../data/regions.RData")

##### shiny App #####
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("lucidus ALA Dashboard 2001-05"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
      #style = "height:90vh",
      column(
        width = 7,
        girafeOutput("map1", height = "700px")
      ),
      column(
        width = 5,
        #plotlyOutput("sunburst2")
        sund2bOutput("sunburst1", height = "700px")
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # data for map
  map_data <- occ_summary |>
    group_by(IBRAIMCRA_region) |>
    summarise(count = sum(count), .groups = "drop") |>
    right_join(regions, by = c("IBRAIMCRA_region" = "region_name")) |>
    st_as_sf(crs = st_crs(regions))
  
  # data for sunburst
  sunburstR_data <- (occ_summary |>
    group_by(kingdom, phylum, class, order) |>
    summarise(count = sum(count), .groups = "drop") |>
    treemap(
      index = c("kingdom", "phylum", "class", "order"),
      vSize = "count",
      draw = FALSE,
      palette = "Dark2"
    ))$tm |>
    arrange(kingdom, phylum, class, order) |>
    d3_nest(value_cols = colnames(tm)[-(1:4)])
  
  # make map
  output$map1 <- renderGirafe({
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
    })
  
  # make sunburst
  output$sunburst1 <- renderSund2b({
    sund2b(
      sunburstR_data,
      colors = htmlwidgets::JS(
        "function(name, d){return d.color || '#ccc';}"
      ),
      valueField = "vSize",
      showLabels = TRUE,
      rootLabel = "RESET",
      breadcrumbs = sund2bBreadcrumb(enabled = FALSE),
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
