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
library(numform)
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
load("../data/taxa_colours.RData")

##### shiny App #####
# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("lucidus ALA Dashboard 2001-05"),
  column(
    width = 12,
    # Sidebar with a slider input for number of bins
    fluidRow(
      #style = "height:90vh",
      column(
        width = 7,
        girafeOutput("map1", height = "700px")
      ),

      column(
        width = 4,
        #plotlyOutput("sunburst2")
        sund2bOutput("sunburstR", height = "700px")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  occ_summary_filtered <- reactive({
    # location filter
    
    # taxa filter
    current_taxa <- sunburstR_selection()
    taxa_deep <- length(current_taxa)
    # year filter
    
    # resource filter
    
    occ_summary |>
      # location filter
      
      # taxa filter
      (\(.) if (taxa_deep == 2) {filter(., kingdom == current_taxa[2])}
       else if (taxa_deep == 3) {filter(., phylum == current_taxa[3])}
       else if (taxa_deep == 4) {filter(., class == current_taxa[4])}
       else if (taxa_deep == 5) {filter(., order == current_taxa[5])}
       else {.})()
      # year filter
    
      # resource filter
  })
  
  
  # data for map
  map_data <- reactive({
    occ_summary_filtered() |>
      group_by(IBRAIMCRA_region) |>
      summarise(count = sum(count), .groups = "drop") |>
      right_join(regions, by = c("IBRAIMCRA_region" = "region_name")) |>
      st_as_sf(crs = st_crs(regions))
  })
  
  sunburstR_data <- (occ_summary |>
       group_by(kingdom, phylum, class, order) |>
       summarise(count = sum(count), .groups = "drop") |>
       treemap(
         index = c("kingdom", "phylum", "class", "order"),
         vSize = "count",
         draw = FALSE,
         palette = "Dark2"
       )
     )$tm |>
    select(-color) |>
    left_join(taxa_colours, by = c("kingdom", "phylum", "class", "order", "level")) |>
    arrange(kingdom, phylum, class, order) |>
    d3_nest(value_cols = c("vSize", "vColor", "stdErr", "vColorValue", "level", "x0","y0", "w", "h", "color"))
    

  # make map
  output$map1 <- renderGirafe({
    int_map <- ggplot() +
      geom_sf_interactive(data = map_data() |> filter(IBRA_IMCRA == "IMCRA"),
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
                           guide = guide_colorsteps(direction = "horizontal",
                                                    label.position = "bottom",
                                                    title.position = "top")) +
      # adds new colour scale
      ggnewscale::new_scale_fill() +
      geom_sf_interactive(data = map_data() |> filter(IBRA_IMCRA == "IBRA"),
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
             opts_toolbar(position = "bottom", hidden = c("zoom_rect"))
           ))
  })

  # make sunburst
  output$sunburstR <- renderSund2b({
    add_shiny(
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
    )
  })

  #sunburst click event
  sunburstR_selection <- reactive({
    input$sunburstR_click
  })
  latestClick <- reactiveVal()
  observeEvent(input$sunburstR_click, {
    latestClick(input$sunburstR_click)
  })
}

# Run the application
shinyApp(ui = ui, server = server)