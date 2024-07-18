library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(anytime)

##Load and prepare the data
df <- read.csv("Data/Bihar_GRD_districts_database.csv")
grd_table <- df %>% filter(date != "NA") 
grd_table$district[grd_table$district == "Saran "] <- "Saran"
str(grd_table)
addFormats("%d-%m-%Y")
grd_table$date <- anydate(grd_table$date)
grd_table$encounter_rate <- round(grd_table$pop_estimate/grd_table$km, 2)
grd_table$sd <- round((grd_table$pop_estimate - grd_table$lower_range)/1.965,2)
grd_table$sd_km <- round(grd_table$sd/grd_table$km, 2)

bihar_shp <- st_read("Data/Districts_Bihar.shp", stringsAsFactors = F)
bihar_simple <- rmapshaper::ms_simplify(bihar_shp, keep = 0.05, keep_shapes = TRUE) ##make the leaflet loading faster

dt_table <- grd_table %>% select(c(district, river, year, survey_date, pop_estimate,
                                   lower_range, upper_range, encounter_rate, sd_km, km, 
                                   status, typepop_survey, data_source))
colnames(dt_table) <- c("District", "River", "Year", "Survey date", "Population size",
                        "Lower range", "Upper range", "Encounter rate", "SD per km",
                        "km", "Status", "Survey method", "Data source"
                        )
dt_table <- data.frame(dt_table, check.names = FALSE) ##uncheck check.names to allow spaces in column names

ui <- page_navbar(
  title = "Ganges River Dolphin Population Tracker",
  theme = bs_theme(version = 5, bootswatch = "zephyr")|> ##setting the primary color of "zephyr" bootswatch theme manually
    bslib::bs_add_rules(
      rules = "
                    .navbar.navbar-default {
                        background-color: $primary !important;
                    }
                    "
    ),
  nav_panel(title = "Trends",
            tags$head(
              tags$script(
                HTML('$(document).ready(function() {
                       $(".navbar .container-fluid")
                         .append("<img id = \'myImage\' src=\'WCTMainLogoWhite_edited.png\' align=\'right\' height = \'57.5px\'>"  );
                      });')),
              tags$style(
                HTML('@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'))),
            layout_columns(
              card(
                card_header(
                  "District and river selection"),
                card_body(
                  min_height = 500,
                  p("Please select a district on the map below"),
                  leafletOutput("mymap"),
                  layout_columns(
                  textInput(inputId = "selecteddistrict", label = "Selected district:", value = ""),
                  selectizeInput(inputId = "river",
                                 label = "Select river(s):",
                                 choices = character(0),
                                 multiple = T))
                  )),
              layout_columns(
                card(
                  full_screen = TRUE,
                  card_header("Graph"),
                  card_body(
                       min_height = 250,
                       max_height = 250,
                       plotlyOutput("graph"))),
                uiOutput("valueboxes"),
                col_widths = c(12)
              )
            )),
  nav_panel(title = "Database", 
            DT::dataTableOutput("table"))
  )


server <- function(input, output, session) {
  data <- reactiveValues(clickedShape = NULL)
  ##Output - Basic map of Bihar
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 86, lat = 26,  zoom = 5.5) %>%
      addPolygons(data = bihar_simple,
                  weight = 1.5,
                  layerId = ~DISTRICT,
                  label = ~paste("District:" , DISTRICT) %>% lapply(htmltools::HTML))
  })

  ##Observe event for adding orange to the district on click
  observeEvent(input$mymap_shape_click, {
        data$clickedShape <- input$mymap_shape_click
        updateTextInput(
          inputId = "selecteddistrict",
          value = paste(unlist(data$clickedShape)[1])) ##Name appears in the 'Selected districts' textbox

        selected_district_df <- reactive(bihar_simple %>%
                                         filter(DISTRICT %in% input$mymap_shape_click$id))
        leafletProxy("mymap") %>%
        addPolygons(data = bihar_simple,
                    weight = 1.5,
                    layerId = ~DISTRICT,
                    label = ~paste("District:" , DISTRICT) %>% lapply(htmltools::HTML)) %>%
        addPolygons(data = selected_district_df(),
                    fillColor = "orange",
                    fillOpacity = 0.8,
                    layerId = ~DISTRICT,
                    label = ~paste("District:" , DISTRICT) %>% lapply(htmltools::HTML))

        updateSelectizeInput( #updates with rivers only associated with selected district
          inputId = "river",
          choices = c(sort(unique(selected_district_table()$river)), ""),
          selected = "")
      })

    selected_district_table <- reactive(grd_table %>%
                                        filter(district %in% input$mymap_shape_click$id))
    
    selected_river_table <- reactive(selected_district_table() %>%
                                     filter(river %in% input$river))
    
    df_river1 <- reactive(selected_river_table() %>%
                          filter(river %in% unlist(input$river)[1]) %>% arrange(desc(date)))
    
    df_river2 <- reactive(selected_river_table() %>%
                          filter(river %in% unlist(input$river)[2]) %>% arrange(desc(date)))
    
    df_river3 <- reactive(selected_river_table() %>%
                          filter(river %in% unlist(input$river)[3]) %>% arrange(desc(date)))
    
    df_river4 <- reactive(selected_river_table() %>%
                            filter(river %in% unlist(input$river)[4]) %>% arrange(desc(date)))
    
    vbs <- reactive(
      list(
      value_box(
      title = tags$p(paste(unlist(input$river)[1], "population -", df_river1()$year_time[1], sep = " "), style = "font-size: 100%;"),
      value = tags$p(paste(df_river1()$pop_estimate[1], "±", df_river1()$sd[1],  sep = " "), style = "font-size: 100%;"),
      theme = "primary", 
      max_height = "80px"
    ), 
    value_box(
      title = tags$p("Distance covered", style = "font-size: 100%;"),
      value = tags$p(paste(df_river1()$km[1], "km", sep = " "), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    ),
    value_box(
      title = tags$p(paste(unlist(input$river)[2], "population -", df_river2()$year_time[1], sep = " "), style = "font-size: 100%;"),
      value = tags$p(paste(df_river2()$pop_estimate[1], "±", df_river2()$sd[1],  sep = " "), style = "font-size: 100%;"),
      theme = "primary",
      max_height = "80px"
    ), 
    value_box(
      title = tags$p("Distance covered", style = "font-size: 100%;"),
      value = tags$p(paste(df_river2()$km[1], "km", sep = " "), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    ),
    value_box(
      title = tags$p(paste(unlist(input$river)[3], "population -", df_river3()$year_time[1], sep = " "), style = "font-size: 100%;"),
      value = tags$p(paste(df_river3()$pop_estimate[1], "±", df_river3()$sd[1],  sep = " "), style = "font-size: 100%;"),
      theme = "primary",
      max_height = "80px"
    ), 
    value_box(
      title = tags$p("Distance covered", style = "font-size: 100%;"),
      value = tags$p(paste(df_river3()$km[1], "km", sep = " "), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    ),
    value_box(
      title = tags$p(paste(unlist(input$river)[4], "population -", df_river4()$year_time[1], sep = " "), style = "font-size: 100%;"),
      value = tags$p(paste(df_river4()$pop_estimate[1], "±", df_river4()$sd[1],  sep = " "), style = "font-size: 100%;"),
      theme = "primary",
      max_height = "80px"
    ), 
    value_box(
      title = tags$p("Distance covered", style = "font-size: 100%;"),
      value = tags$p(paste(df_river4()$km[1], "km", sep = " "), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    )
    ))
    
    ##Output - Graph
    output$graph <- renderPlotly({
        req(input$river)
        graph_df <- selected_river_table() %>% arrange(date)
        x_axis_breaks  <- seq(
          from = min(graph_df$date), 
          to = max(graph_df$date),
          by = "1 year"
        )   
        
        tickvals  <- format(x_axis_breaks, "%Y")
        ticktext <- format(x_axis_breaks, "%Y")
        
        graph_df %>%
           plot_ly(x = ~date, 
                   y = ~encounter_rate, 
                   type = 'scatter', 
                   mode = 'lines+markers', 
                   color = ~river,
                   error_y = list(array = ~sd_km),
                   hovertemplate = paste('Year: %{x|%Y}',
                                         '<br>Encounter rate: %{y}<extra></extra>')) %>%
           layout(
            yaxis = list(title = "Encounter rate <br> (no. of dolphins/km)"),
            xaxis = list(
                         title = "Survey Year",
                         tickvals = tickvals,
                         ticktext = ticktext,
                         tickmode = "array",
                         tickangle = -90),
            legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.5)
          )
      })
    
    output$valueboxes <- renderUI({
      if(length(unlist(input$river)) == 1){
        layout_columns(
            vbs()[[1]], vbs()[[2]],
            col_widths = c(6, 6))
      }
      else if(length(unlist(input$river)) == 2){
        layout_columns(
        vbs()[[1]], vbs()[[2]], vbs()[[3]], vbs()[[4]],
        col_widths = c(6, 6, 6, 6))
      } 
      else if(length(unlist(input$river)) == 3){
        layout_columns(
        vbs()[[1]], vbs()[[2]], vbs()[[3]], vbs()[[4]], vbs()[[5]], vbs()[[6]],
        col_widths = c(6, 6, 6, 6, 6, 6))
      } else if(length(unlist(input$river)) == 4){
        layout_columns(
          vbs()[[1]], vbs()[[2]], vbs()[[3]], vbs()[[4]], vbs()[[5]], vbs()[[6]], vbs()[[7]], vbs()[[8]], 
          col_widths = c(6, 6, 6, 6, 6, 6, 6, 6))
      }
      })
    
    output$table <- DT::renderDataTable({
      if(is.null(input$selecteddistrict) == FALSE & isTruthy(input$river)){
        dt_table %>% filter(District %in% unlist(input$selecteddistrict)) %>% arrange(River, Year)
      } 
      else if(isTruthy(input$selecteddistrict) & isTruthy(input$river)){
        dt_table %>% filter(District %in% unlist(input$selecteddistrict)) %>%
          filter(River %in% unlist(input$river)) %>% arrange(River, Year)
      }
      else {
        dt_table %>% arrange(District, River, Year)
      } ##isTruthy to find out if value is truthy i.e it is not FALSE, NULL, "" or an empty vector
    })
}

shinyApp(ui, server)