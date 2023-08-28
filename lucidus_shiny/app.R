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
library(glue)
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
    fluidRow(
      column(
        width = 2,
        actionButton("reset_map", label = "Reset Map Selection")
      ),
      column(width = 8),
      column(
        width = 2,
        actionButton("reset_sunburst", label = "Reset Taxa Selection")
      )
    ),
    # Sidebar with a slider input for number of bins
    fluidRow(
      #style = "height:90vh",
      column(
        width = 7,
        girafeOutput("ggiraph_map", height = "700px")
      ),

      column(
        width = 5,
        plotlyOutput("sunburstplotly", height = "700px")
        #sund2bOutput("sunburstR", height = "700px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  occ_summary_filtered_for_map <- reactive({
    # taxa filter
    current_taxa <- str_split(current_level(), " ")[[1]] %>% 
      replace(grepl("\\[UNIDENTIFIED\\]", .), NA)
    taxa_deep <- length(current_taxa)
    # year filter
    # resource filter
    
    
    occ_summary |>
      # taxa filter
      (\(.) if (taxa_deep == 1 & current_taxa[1] == "TOTAL") {.}
       else if (taxa_deep == 1) {filter(., match(kingdom, current_taxa[1]) == 1)}
       else if (taxa_deep == 2) {filter(., match(kingdom, current_taxa[1]) == 1,
                                           match(phylum, current_taxa[2]) == 1)}
       else if (taxa_deep == 3) {filter(., match(kingdom, current_taxa[1]) == 1,
                                           match(phylum, current_taxa[2]) == 1,
                                           match(class, current_taxa[3]) == 1)}
       else if (taxa_deep == 4) {filter(., match(kingdom, current_taxa[1]) == 1,
                                           match(phylum, current_taxa[2]) == 1,
                                           match(class, current_taxa[3]) == 1,
                                           match(order, current_taxa[4]) == 1)}
       else {.})()
      # year filter
    
      # resource filter
  })
  
  occ_summary_filtered_for_sunburst <- reactive({
    # location filter
    selected_regions <- map_selected()
    # year filter
    # resource filter
    
    
    occ_summary |>
      # location filter
      (\(.) if (length(selected_regions) == 0) {.}
       else {filter(., IBRAIMCRA_region %in% selected_regions)})()
      # year filter
    
      # resource filter
  })
  
  
  # data for map
  map_data <- reactive({
    occ_summary_filtered_for_map() |>
      group_by(IBRAIMCRA_region) |>
      summarise(count = sum(count), .groups = "drop") |>
      right_join(regions, by = c("IBRAIMCRA_region" = "region_name")) |>
      st_as_sf(crs = st_crs(regions))
  })
  
  # data for sunburstR sunburst
  {
    sunburstR_data <- 
      (occ_summary |>
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
  }
  
  # data for plotly sunburst
  sunburstplotly_data <- reactive({
    tree_data <- occ_summary_filtered_for_sunburst() |>
      group_by(kingdom, phylum, class, order) |>
      summarise(count = sum(count), .groups = "drop") |>
      replace_na(list(kingdom = "[UNIDENTIFIED]",
                      phylum = "[UNIDENTIFIED]",
                      class = "[UNIDENTIFIED]",
                      order = "[UNIDENTIFIED]"))
    
    rbind(
      tree_data |>
        mutate(id = paste(kingdom, phylum, class, order, " "),
               parent = paste(kingdom, phylum, class, order),
               names = "",
               count = 0,
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
  })

  # make ggiraph map
  output$ggiraph_map <- renderGirafe({
    ggiraph_map_data <- map_data()
    # IMCRA scale
    IMCRA_counts <- ggiraph_map_data |> filter(IBRA_IMCRA == "IMCRA") |> pull(count)
    max_IMCRA <- ifelse(all(is.na(IMCRA_counts)), 0, max(IMCRA_counts, na.rm = TRUE))
    IMCRA_limit <- max(ceiling(max_IMCRA / 10^(nchar(max_IMCRA) - 1)) * 10^(nchar(max_IMCRA) - 1), 5)
    IMCRA_scale <- seq(0, sqrt(IMCRA_limit), length.out = 6)^2
    IMCRA_labels <- f_denom(IMCRA_scale, mix_denom = TRUE)
    print(IMCRA_labels)
    # IBRA scale
    IBRA_counts <- ggiraph_map_data |> filter(IBRA_IMCRA == "IBRA") |> pull(count)
    max_IBRA <- ifelse(all(is.na(IBRA_counts)), 0, max(IBRA_counts, na.rm = TRUE))
    IBRA_limit <- max(ceiling(max_IBRA / 10^(nchar(max_IBRA) - 1)) * 10^(nchar(max_IBRA) - 1), 5)
    IBRA_scale <- seq(0, sqrt(IBRA_limit), length.out = 6)^2
    IBRA_labels <- f_denom(IBRA_scale, mix_denom = TRUE)
    print(IBRA_labels)
    # plot
    int_map <- ggplot() +
      {if (max_IMCRA > 0) {
        geom_sf_interactive(data = ggiraph_map_data |> filter(IBRA_IMCRA == "IMCRA"),
                            aes(fill = count,
                                tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                            colour = "gray30")
        } else {
          geom_sf_interactive(data = ggiraph_map_data |> filter(IBRA_IMCRA == "IMCRA"),
                              aes(tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                              fill = "grey90", colour = "gray30")
        }} +
      {if (max_IMCRA > 0) {
        scale_fill_distiller(name = "IMCRA",
                             type = "seq",
                             palette = "PuBu",
                             na.value = "grey90",
                             direction = 1,
                             trans = "sqrt",
                             limits = c(IMCRA_scale[1], IMCRA_scale[6]),
                             breaks = IMCRA_scale,
                             #labels = IMCRA_labels,
                             guide = guide_colorsteps(direction = "horizontal",
                                                      label.position = "bottom",
                                                      title.position = "top"))
      }} +
      # adds new colour scale
      {if (max_IMCRA > 0 & max_IBRA > 0) ggnewscale::new_scale_fill()} +
      {if (max_IBRA > 0) {
        geom_sf_interactive(data = ggiraph_map_data |> filter(IBRA_IMCRA == "IBRA"),
                            aes(fill = count,
                                tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                            colour = "gray10")
      } else {
        geom_sf_interactive(data = ggiraph_map_data |> filter(IBRA_IMCRA == "IBRA"),
                            aes(tooltip = IBRAIMCRA_region, data_id = IBRAIMCRA_region),
                            fill = "gray90", colour = "gray10")
      }} +
      {if (max_IBRA > 0) {
        scale_fill_distiller(name = "IBRA",
                             type = "seq",
                             palette = "YlOrBr",
                             na.value = "grey90",
                             direction = 1,
                             trans = "sqrt",
                             limits = c(IBRA_scale[1], IBRA_scale[6]),
                             breaks = IBRA_scale,
                             #labels = IBRA_labels,
                             guide = guide_colorsteps(direction = "horizontal",
                                                      label.position = "bottom",
                                                      title.position = "top"))
      }} +
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
             opts_selection(selected = map_selected(),
                            css = "stroke-width:0.3;",
                            type = "multiple",
                            only_shiny = FALSE),
             opts_selection_inv(css = "opacity:0.3;"),
             opts_hover(css = "stroke-width:1;stroke:black"),
             opts_zoom(max = 5),
             opts_toolbar(position = "bottom", hidden = c("zoom_rect"))
           ))
  })
  
  # reactivity for ggiraph map
  map_selected <- reactive({
    input$ggiraph_map_selected
  })
  
  # make sunburstR sunburst
  {
    # output$sunburstR <- renderSund2b({
    #   add_shiny(
    #     sund2b(
    #       sunburstR_data,
    #       colors = htmlwidgets::JS(
    #         "function(name, d){return d.color || '#ccc';}"
    #       ),
    #       valueField = "vSize",
    #       showLabels = TRUE,
    #       rootLabel = "RESET",
    #       breadcrumbs = sund2bBreadcrumb(enabled = FALSE),
    #     )
    #   )
    # })
    # 
    # #sunburst click event
    # sunburstR_selection <- reactive({
    #   input$sunburstR_click
    # })
    # latestClick <- reactiveVal()
    # observeEvent(input$sunburstR_click, {
    #   latestClick(input$sunburstR_click)
    # })
  }
  
  # make plotly sunburst
  output$sunburstplotly <- renderPlotly({
    plot_ly(
      data = sunburstplotly_data(),
      type = "sunburst",
      ids = ~id,
      labels = ~names,
      parents = ~parent,
      values = ~count,
      branchvalues = "total",
      maxdepth = 5,
      insidetextorientation = "radial",
      sort = FALSE,
      rotation = 90,
      hovertemplate = ~hovertemplate,
      marker = list(colors = ~color),
      source = "sunburst_source",
      customdata = ~id,
      level = current_level()
    ) |>
      layout(colorway = ~color) |>
      event_register("plotly_sunburstclick")
  })
  
  # reactivity for plotly sunburst
  sunburst_click <- reactive({
    unlist(event_data(event = "plotly_sunburstclick", source = "sunburst_source", priority = "event"))[3]
  })
  
  previous_click <- reactiveVal("TOTAL")
  current_level <- reactiveVal("TOTAL")

  observeEvent(sunburst_click(), {

    sunburst_click_holder <- sunburst_click()
    previous_click_holder <- previous_click()
    
    # if we changed selection of a leaf then update the current level to that
    if (sunburst_click_holder == "TOTAL" & previous_click_holder == "TOTAL") {
      
    } else if (sunburst_click_holder != previous_click_holder) {
      current_levels_string <- str_split(sunburst_click_holder, " ")[[1]]
      current_level(str_flatten(current_levels_string[1:length(current_levels_string)], collapse = " "))
    } else if (sunburst_click_holder == previous_click_holder) {
      current_levels_string <- str_split(previous_click_holder, " ")[[1]]
      if (length(current_levels_string) == 1) {
        current_level("TOTAL")
      } else {
        current_level(str_flatten(current_levels_string[1:(length(current_levels_string) - 1)], collapse = " "))
      }
    }

    previous_click(current_level())

  })
}

# Run the application
shinyApp(ui = ui, server = server)