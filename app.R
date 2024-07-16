library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(anytime)

##Load and prepare the data
df <- read.csv("Data/Bihar_GRD_districts_database.csv")
grd_table <- df %>% filter(date != "NA") %>%
             select(!c(wkt_geom, ST_NM, ST_CEN_CD, DT_CEN_CD, censuscode))
str(grd_table)
addFormats("%d-%m-%Y")
grd_table$date <- anydate(grd_table$date)
grd_table$encounter_rate <- grd_table$pop_estimate/grd_table$km
grd_table$sd <- (grd_table$pop_estimate - grd_table$lower_range)/1.965
grd_table$sd_km <- grd_table$sd/grd_table$km

bihar_shp <- st_read("Data/Districts_Bihar.shp", stringsAsFactors = F)
bihar_simple <- rmapshaper::ms_simplify(bihar_shp, keep = 0.05, keep_shapes = TRUE) ##make the leaflet loading faster

ui <- page_navbar(
  title = "Ganges River Dolphin District Portal",
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
                  textInput(inputId = "selecteddistrict", label = "Selected district:", value = ""),
                  selectizeInput(inputId = "river",
                                 label = "Select river(s):",
                                 choices = character(0),
                                 multiple = T)
                  )),
              layout_columns(
                card(
                  full_screen = TRUE,
                  card_header("Graph"),
                  card_body(
                       min_height = 250,
                       plotlyOutput("graph"))),
                card(
                  full_screen = TRUE,
                  card_header("Table"),
                  card_body(
                    min_height = 250,
                    DT::dataTableOutput("table"))),
                col_widths = c(12, 12) #meaning each of the cards will occupy that entire 12-
              )
            )),
  nav_panel(title = "Instructions on use", p("Content to be added"))
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
                   error_y = list(array = ~sd_km)) %>%
           layout(
            yaxis = list(title = "Encounter rate <br> (no. of individuals/km)"),
            xaxis = list(title = "Date of survey",
                         tickvals = tickvals,
                         ticktext = ticktext,
                         tickmode = "array",
                         tickangle = -90)
          )
      })

    ##Output - Data table
    output$table <- DT::renderDataTable({
      req(input$river) ##req will execute the render table only when input$river is not equal to NULL
      selected_district_table() %>% filter(river %in% unlist(input$river))
})
}

shinyApp(ui, server)