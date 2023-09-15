#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##### Libraries #####
library(ggiraph)
library(ggthemes)
library(glue)
library(htmltools)
library(monochromeR)
library(packcircles)
library(plotly)
library(prismatic)
library(RColorBrewer)
library(scales)
library(sf)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(treemap)

##### Load Data #####
load("../data/occ_summary120.rds")
load("../data/aus_outline.rds")
load("../data/regions.rds")
load("../data/taxa_colours.rds")

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
        width = 2,
        box(
          title = h5(strong("SELECTED REGIONS")),
          style = "width:250px;height:600px;overflow-y:auto;",
          htmlOutput("text_map")
        )
      ),
      column(
        width = 5,
        girafeOutput("ggiraph_map", height = "700px")
      ),

      column(
        width = 4,
        plotlyOutput("sunburstplotly", height = "700px")
        #sund2bOutput("sunburstR", height = "700px")
      ),
      column(width = 1)
    ),
    # Barplot and circle plot (?)
    fluidRow(
      column(width = 1),
      column(
        width = 6,
        girafeOutput("ggiraph_barplot_div", height = "450px", width = "100%")
      ),
      column(
        width = 4,
        girafeOutput("ggiraph_circles", height = "450px", width = "100%")
      ),
      column(width = 1)
    ),
    fluidRow(
      column(
        width = 2,
        actionButton("reset_bar_div", label = "Reset Year Selection")
      ),
      column(width = 8),
      column(
        width = 2,
        actionButton("reset_circles", label = "Reset Basis Selection")
      )
    )
  )
)

server <- function(input, output, session) {
  
  ###### Filter occ_summary for plots ######
  occ_summary_filtered_for_map <- reactive({
    # taxa filter
    current_taxa <- str_split(current_level(), " ")[[1]] %>% 
      replace(grepl("\\[UNIDENTIFIED\\]", .), NA)
    taxa_deep <- length(current_taxa)
    # year filter
    selected_years <- barplot_selected()
    # basis filter
    selected_basis <- gsub("\n", " ", circles_selected())
    
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
       else {.})() |>
      # year filter
      (\(.) if (length(selected_years) == 0) {.}
       else {filter(., year %in% selected_years)})() |>
      # basis filter
      (\(.) if (length(selected_basis) == 0) {.}
       else {filter(., basisOfRecord %in% selected_basis)})()
  })
  
  occ_summary_filtered_for_sunburst <- reactive({
    # location filter
    selected_regions <- map_selected()
    # year filter
    selected_years <- barplot_selected()
    # basis filter
    selected_basis <- gsub("\n", " ", circles_selected())
    
    occ_summary |>
      # location filter
      (\(.) if (length(selected_regions) == 0) {.}
       else {filter(., IBRAIMCRA_region %in% selected_regions)})()  |>
      # year filter
      (\(.) if (length(selected_years) == 0) {.}
       else {filter(., year %in% selected_years)})() |>
      # basis filter
      (\(.) if (length(selected_basis) == 0) {.}
       else {filter(., basisOfRecord %in% selected_basis)})()
  })
  
  occ_summary_filtered_for_barplot <- reactive({
    # taxa filter
    current_taxa <- str_split(current_level(), " ")[[1]] %>% 
      replace(grepl("\\[UNIDENTIFIED\\]", .), NA)
    taxa_deep <- length(current_taxa)
    # location filter
    selected_regions <- map_selected()
    # basis filter
    selected_basis <- gsub("\n", " ", circles_selected())
    
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
       else {.})() |>
      # location filter
      (\(.) if (length(selected_regions) == 0) {.}
       else {filter(., IBRAIMCRA_region %in% selected_regions)})() |>
      # basis filter
      (\(.) if (length(selected_basis) == 0) {.}
       else {filter(., basisOfRecord %in% selected_basis)})()
  })
  
  occ_summary_filtered_for_circles <- reactive({
    # taxa filter
    current_taxa <- str_split(current_level(), " ")[[1]] %>% 
      replace(grepl("\\[UNIDENTIFIED\\]", .), NA)
    taxa_deep <- length(current_taxa)
    # location filter
    selected_regions <- map_selected()
    # year filter
    selected_years <- barplot_selected()
    
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
       else {.})() |>
      # location filter
      (\(.) if (length(selected_regions) == 0) {.}
       else {filter(., IBRAIMCRA_region %in% selected_regions)})() |>
      # year filter
      (\(.) if (length(selected_years) == 0) {.}
       else {filter(., year %in% selected_years)})()
  })
  
  ###### Produce data required for plots ######
  # data for map
  map_data <- reactive({
    occ_summary_filtered_for_map() |>
      group_by(IBRAIMCRA_region) |>
      summarise(count = sum(count), .groups = "drop") |>
      right_join(regions, by = c("IBRAIMCRA_region" = "region_name")) |>
      st_as_sf(crs = st_crs(regions)) |>
      mutate(count_label = ifelse(is.na(count), "0", prettyNum(count, big.mark = ",")))
  })
  
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

  # data for barplot
  barplot_data <- reactive({
    # occ_summary_filtered_for_barplot() |>
    #   group_by(year) |>
    #   summarise(count = sum(count), .groups = "drop")
  })
  # data for div barplot
  barplot_data_div <- reactive({
    occ_summary_filtered_for_barplot() |>
      group_by(year, cs) |>
      summarise(count = sum(count), .groups = "drop") |>
      mutate(count = ifelse(cs == "Non-citizen Science", -1 * count, count),
             count_label = ifelse(is.na(count), "0", prettyNum(abs(count), big.mark = ",")))
  })
  
  # data for packed circles
  circles_data <- reactive({
    occ_summary_filtered_for_circles() |>
      group_by(basisOfRecord) |>
      summarise(count = sum(count), .groups = "drop") |>
      mutate(basisOfRecord_text = gsub(" ", "\n", basisOfRecord)) |>
      arrange(desc(count)) |>
      mutate(fill = generate_palette(current_colour(), "go_lighter", n_colours = 13)[1:n()],
             colour = ifelse(clr_extract_luminance(fill) > 50, "black", "white"),
             count_label = ifelse(is.na(count), "0", prettyNum(count, big.mark = ",")))
  })
  
  ###### Make plots ######
  # make ggiraph map
  output$ggiraph_map <- renderGirafe({
    ggiraph_map_data <- map_data()
    # IMCRA scale
    map_counts <- ggiraph_map_data |> pull(count)
    max_counts <- ifelse(all(is.na(map_counts)), 0, max(map_counts, na.rm = TRUE))
    counts_limit <- max(ceiling(max_counts / 10^(nchar(max_counts) - 1)) * 10^(nchar(max_counts) - 1), 5)
    counts_scale <- seq(0, sqrt(counts_limit), length.out = 6)^2
    counts_labels <- label_number(accuracy = 0.1, scale_cut = cut_short_scale())(counts_scale) %>%
                      {gsub("\\.0", "", .)}
    # plot
    int_map <- ggplot() +
      geom_sf_interactive(data = ggiraph_map_data,
                          aes(fill = count,
                              tooltip = sprintf("%s\n %s", IBRAIMCRA_region, count_label), 
                              data_id = IBRAIMCRA_region),
                          colour = "grey30") +
      scale_fill_gradientn(name = "Counts",
                           colours = generate_palette(current_colour(), "go_both_ways", 9)[1:8],
                           na.value = "grey95",
                           trans = "sqrt",
                           limits = c(counts_scale[1], counts_scale[6]),
                           breaks = counts_scale,
                           labels = counts_labels,
                           guide = guide_colorsteps(direction = "horizontal",
                                                    label.position = "bottom")) +
      geom_sf(data = aus_outline,
              colour = alpha("grey30", 
                             ifelse(is.null(map_selected()), 1, 0.3)), 
              fill = NA, 
              linewidth = 0.3) +
      coord_sf(xlim = c(110, 155), ylim = c(-45, -10)) +
      theme_void() +
      theme(legend.box = "vertical",
            legend.position = c(0.3, 0.175),
            legend.title = element_blank(),
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
             opts_toolbar(position = "topleft", hidden = c("zoom_rect"))
           ))
  })
  
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
      maxdepth = 2,
      insidetextorientation = "radial",
      sort = FALSE,
      rotation = 90,
      hovertemplate = ~hovertemplate,
      hoverlabel = list(align = "left"),
      marker = list(colors = ~color),
      source = "sunburst_source",
      customdata = ~id,
      level = current_level()
    ) |>
      layout(colorway = ~color) |>
      event_register("plotly_sunburstclick")
  })
  
  # make ggiraph barplot (non-diverging)
  output$ggiraph_barplot <- renderGirafe({
    # ggiraph_barplot_data <- barplot_data()
    # 
    # barplot_fill <- sunburstplotly_data() |> 
    #   filter(id == current_level()) |> 
    #   pull(color)
    # 
    # int_barplot <- ggplot(ggiraph_barplot_data) +
    #   geom_bar_interactive(aes(x = year, y = count,
    #                            tooltip = year, data_id = year),
    #                        fill = ifelse(barplot_fill == "#FFFFFF", "#bebada", barplot_fill), 
    #                        alpha = 0.9,
    #                        stat = "identity", position = "identity") +
    #   scale_y_continuous(breaks = pretty(c(0, ggiraph_barplot_data$count)),
    #                      labels = label_number(
    #                        accuracy = 0.1, 
    #                        scale_cut = cut_short_scale())(pretty(c(0, ggiraph_barplot_data$count))) %>%
    #                        {gsub("\\.0", "", .)},
    #                      expand = c(0, 0)) +
    #   scale_x_continuous(limits = c(min(ggiraph_barplot_data$year) - 1, 
    #                                 max(ggiraph_barplot_data$year) + 1),
    #                      breaks = pretty(ggiraph_barplot_data$year, 
    #                                      n = diff(range(ggiraph_barplot_data$year)) / 10)) +
    #   xlab("Year") +
    #   ylab("Number of Occurrences") +
    #   theme_classic()
    # girafe(ggobj = int_barplot,
    #        options = list(
    #          opts_selection(css = "stroke-width:0.3;",
    #                         type = "multiple",
    #                         only_shiny = FALSE),
    #          opts_selection_inv(css = "opacity:0.3;"),
    #          opts_hover(css = "stroke-width:1;stroke:black"),
    #          opts_toolbar(position = "top"),
    #          opts_sizing(rescale = FALSE)
    #        ),
    #        width_svg = 14, height_svg = 6
    #       )
  })
  # make ggiraph barplot (diverging)
  output$ggiraph_barplot_div <- renderGirafe({
    ggiraph_barplot_data_div <- barplot_data_div()
    
    barplot_fill <- current_colour()
    
    barplot_fill_values <- c(barplot_fill, generate_palette(barplot_fill, "go_both_ways", 9)[3]) |>
      setNames(c("Citizen Science", "Non-citizen Science"))
    
    int_barplot <- ggplot(ggiraph_barplot_data_div) +
      geom_bar_interactive(aes(x = year, y = count, fill = cs,
                               tooltip = glue("{year} {cs} \n {count_label}"), 
                               data_id = year),
                           stat = "identity", position = "identity") +
      scale_fill_manual(name = "Data Resource",
                        values = barplot_fill_values) +
      scale_y_continuous(breaks = pretty(c(0, ggiraph_barplot_data_div$count)),
                         labels = label_number(
                           accuracy = 0.1, 
                           scale_cut = cut_short_scale())(abs(pretty(c(0, ggiraph_barplot_data_div$count)))) %>%
                           {gsub("\\.0", "", .)}) +
      scale_x_continuous(# limits = c(min(ggiraph_barplot_data_div$year) - 1, 
                         #            max(ggiraph_barplot_data_div$year) + 1),
                         limits = c(1899.5, 2020.5),
                         breaks = pretty(ggiraph_barplot_data_div$year, 
                                         n = diff(range(ggiraph_barplot_data_div$year)) / 10)) +
      xlab("Year") +
      ylab("Number of Occurrences") +
      theme_classic() +
      theme(axis.title = element_text(size = 20))
    girafe(ggobj = int_barplot,
           options = list(
             opts_selection(selected = barplot_selected(),
                            css = "stroke-width:0.3;",
                            type = "multiple",
                            only_shiny = FALSE),
             opts_selection_inv(css = "opacity:0.3;"),
             opts_hover(css = "stroke-width:1;stroke:black"),
             opts_toolbar(position = "bottomleft"),
             opts_zoom(max = 5),
             opts_sizing(rescale = FALSE)
           ),
           width_svg = 14, height_svg = 6
    )
  })
  
  # make packed circles plot
  output$ggiraph_circles <- renderGirafe({
    ggiraph_circles_data <- circles_data()
    
    circular_text_data <- ggiraph_circles_data |>
      cbind(circleProgressiveLayout(ggiraph_circles_data$count, sizetype = "area"))
    
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
      scale_size(range = c(min(circular_text_data$radius), 
                           max(circular_text_data$radius)) * 
                         (6 / max(circular_text_data$radius))) +
      theme_void() + 
      theme(legend.position = "none", 
            plot.margin = unit(c(0, 0, 0, 0), "cm"),
            plot.background = element_rect(fill = NA, colour = NA),
            panel.background = element_rect(fill = NA, colour = NA)) + 
      coord_equal()
    girafe(ggobj = int_circular_plot,
           options = list(
             opts_selection(selected = circles_selected(),
                            css = "stroke-width:1;stroke:black",
                            type = "multiple",
                            only_shiny = FALSE),
             opts_selection_inv(css = "opacity:0.3;"),
             opts_hover(css = "stroke-width:1;stroke:black"),
             opts_toolbar(position = "bottomright"),
             opts_zoom(max = 5),
             opts_sizing(rescale = TRUE)
           ))
  })
  
  ###### Plot reactivities ######
  # current colour
  current_colour <- reactive({
    plot_colour <- sunburstplotly_data() |> 
      filter(id == current_level()) |> 
      pull(color)
    
    ifelse(plot_colour == "#FFFFFF", "#817E94", plot_colour)
  })
  
  # reactivity for ggiraph map
  map_selected <- reactive({
    if (map_reset_status()) {
      NULL
    } else {
      input$ggiraph_map_selected
    }
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
  
  # reactivity for barplot_div
  barplot_selected <- reactive({
    input$ggiraph_barplot_div_selected
  })
  
  # reactivity for packed circles
  circles_selected <- reactive({
    input$ggiraph_circles_selected
  })
  
  ###### Reset Buttons ######
  # reset map
  map_reset_status <- reactiveVal(FALSE)
  # observeEvent(input$reset_map, {
  #   map_reset_status(TRUE)
  # })
  # reset sunburst
  
  # reset barplot_div
  
  # reset packed circles
  
  ###### Text Output ######
  # text for map
  output$text_map <- renderUI({
    c(
      paste(sort(map_selected()), collapse = "<br>")
    ) |>
      HTML()
  })
}

# Run the application
shinyApp(ui = ui, server = server)