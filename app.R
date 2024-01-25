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
               leafletOutput("mymap", height = 600))
           ),
    column(width = 4,
           box(width = NULL,
           p(
             "The purpose of this app is to view the population estimate of Ganges River Dolphins per district. The data ranges from 2013 to 2023.
              Users can click on a district in the map to view the population estimate of Ganges River Dolphins for each river flowing through that district."
           ),
           uiOutput("graph"),
           actionButton("reset", "Reset")
           )
    )
  )
 )
)

server <- function(input, output, session) {
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
}

shinyApp(ui = ui, server = server)
