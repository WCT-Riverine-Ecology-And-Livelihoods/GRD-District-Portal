library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)

##Load the data
grd_table <- read.csv("Data/GRD_districts_table_v2.csv")
grd_states <- grd_table %>% distinct(ST_NM)
grd_districts <- grd_table %>% distinct(DISTRICT)
grd_india_shp <- st_read("Data/grd_india_table.shp", stringsAsFactors = F)
grd_shp_filtered <- grd_india_shp %>% 
                    filter(STATE %in% grd_states$ST_NM) %>%
                    filter(DISTRICT %in% grd_districts$DISTRICT)
  
##Shiny 
ui <- dashboardPage(
  dashboardHeader(title = "GRD Trends District Portal"),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
    column(width = 8,
           box(width = NULL, solidHeader = TRUE, 
               leafletOutput("mymap", height = 500))
           ),
    column(width = 4,
           box(width = NULL, 
           p(
             "The purpose of this app is to view the population estimate of Ganges River Dolphins per district. The data ranges from 2013 to 2023.
              Users can click on a district in the map to view the population estimate of Ganges River Dolphins for each river flowing through that district."
           ),
           textInput(inputId = "selecteddistrict", label = "Selected district:", value = ""),
           plotlyOutput("graph", height = 300)
           )
    )
  )
 )
)

server <- function(input, output, session) {
  data <- reactiveValues(clickedShape = NULL) 
  ##Output - Map
  output$mymap <- renderLeaflet({
      leaflet() %>%
      addTiles() %>%
      setView(lng = 82.9, lat = 25.7,  zoom = 6) %>% 
      addPolygons(data = grd_shp_filtered,
                  weight = 1.5,
                  layerId = ~DISTRICT,
                  label = ~paste("District:" , DISTRICT,
                                 "<br>State:", STATE) %>% lapply(htmltools::HTML)) 
  })
  
  observeEvent(input$mymap_shape_click, { ##for updating graph on click of polygon in map
    data$clickedShape <- input$mymap_shape_click
    updateTextInput(inputId = "selecteddistrict", value = paste(unlist(data$clickedShape)[1]))
    selected_district_df <- reactive(grd_shp_filtered %>%
                                  filter(DISTRICT %in% input$mymap_shape_click$id))
    leafletProxy("mymap") %>% 
    addPolygons(data = grd_shp_filtered,
                  weight = 1.5,
                  layerId = ~DISTRICT,
                  label = ~paste("District:" , DISTRICT,
                                 "<br>State:", STATE) %>% lapply(htmltools::HTML)) %>%  
    addPolygons(data = selected_district_df(),
                fillColor = "orange",
                fillOpacity = 0.8,
                layerId = ~DISTRICT,
                label = ~paste("District:" , DISTRICT,
                               "<br>State:", STATE) %>% lapply(htmltools::HTML))
  })
  
  selected_district_table <- reactive(grd_table %>%
                                      filter(DISTRICT %in% input$mymap_shape_click$id))
  output$graph <- renderPlotly({
    selected_district_table() %>%
      plot_ly() %>%
      add_trace(x = ~river, y = ~GRD_pop, type = 'bar', width = 0.3, color = "orange") %>%
      layout(
        xaxis = list(title = "River"),
        yaxis = list(title = "GRD Population Estimate")
      )
  })
}

shinyApp(ui = ui, server = server)
