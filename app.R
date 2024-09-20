library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(sf)
library(dplyr)
library(lubridate)

##Load and prepare the data
df <- read.csv("Data/Bihar_GRD_districts_database.csv")
surveymethod_table <- read.csv("Data/Surveymethod.csv")
grd_table <- df %>% filter(date != "NA") 
grd_table$district[grd_table$district == "Saran "] <- "Saran"
str(grd_table)
grd_table$date <- as.Date(grd_table$date, format = "%d-%m-%Y")
grd_table$survey_month <- format(grd_table$date, "%B")
grd_table$encounter_rate <- round(grd_table$pop_estimate/grd_table$km, 2)
grd_table$sd <- round((grd_table$pop_estimate - grd_table$lower_range)/1.965,2)
grd_table$sd_km <- round(grd_table$sd/grd_table$km, 2)
grd_table$pop_size <- paste(grd_table$pop_estimate, paste0("(", grd_table$lower_range, "-", grd_table$upper_range, ")"))
  
##Load shapefiles and add color palette to differentiate districts with and withour GRDs
bihar_shp <- st_read("Data/Districts_Bihar.shp", stringsAsFactors = F)
bihar_shp$GRD_presence <- as.factor(ifelse(bihar_shp$DISTRICT %in% unique(grd_table$district), "Present", "Absent"))
bihar_simple <- rmapshaper::ms_simplify(bihar_shp, keep = 0.05, keep_shapes = TRUE) ##make the leaflet loading faster
GRD_presence_pal <- leaflet::colorFactor(c("gray37", "#3459e6"), bihar_shp$GRD_presence)

##Prepare table for the 'Database' tab
dt_table <- grd_table %>% select(c(district, river, year, survey_month, pop_size,
                                   km, encounter_rate, sd_km, status, typepop_survey, data_source))
colnames(dt_table) <- c("District", "River", "Year", "Survey month", "Population size (Range)", "River length (km)", "Encounter rate", 
                        "Standard deviation (per km)", "Status", "Survey method", "Data source"
                        )
dt_table <- data.frame(dt_table,  row.names = NULL, check.names = FALSE) ##uncheck check.names to allow spaces in column names

##Prepare survey method table for the 'About' tab
colnames(surveymethod_table) <- c("Survey method", "Interpretation of population and error reported",  "Data quality")
surveymethod_table <- data.frame(surveymethod_table, check.names = FALSE) ##likewise uncheck for survey method table

##ui
ui <- page_navbar(
  title = "Ganges River Dolphin Population Tracker: Bihar",
  theme = bs_theme(version = 5, bootswatch = "zephyr")|>
    bslib::bs_add_rules( ##adding most css rules for different elements here
      rules = "
      .navbar {font-family: Arial; font-size: 20px;} /* change font of nav bar */
      .navbar-default .navbar-nav {font-family: Arial; font-size: 15px;} /* change font of navbar navpanel */
      .navbar.navbar-default {background-color: $primary !important;} /* set the primary color of zephyr bootswatch theme */
      .body {font-family: Arial, sans-serif; background-color: #f5f5f5;} /* change font family of body text and background color */
      h4 {margin-top: 0px; margin-bottom: 0px;}
      p {margin-bottom: 0px;}
      ol {margin-bottom: 0px;}
      p + h4 {margin-top: 0px;}"
    ),
  tags$head(
    tags$style(HTML("
      div.nopad .value-box-area {  /* remove padding around the card body for value boxes */
        padding: 0;
      }
    "))),
  nav_panel(title = "Trends",
            tags$head(
              tags$script(
                HTML('$(document).ready(function() {
                       $(".navbar .container-fluid")
                         .prepend("<img id = \'applogo\' src=\'AppLogo_Final.png\' align=\'left\' height = \'80px\' style=\'margin-right:10px;\'>")
                         .append("<img id = \'WCTlogo\' src=\'WCTLogo.png\' align=\'right\' height = \'60px\'>");
                      });')),
              tags$style(
                HTML('@media (max-width:992px) { 
                     #Applogo { position: fixed; left: 10%; top: 0%; }
                     #WCTlogo { position: fixed; right: 10%; top: 0.5%; }
                     }'))),
            layout_columns(
              card(
                card_header(
                  "District and river selection"),
                card_body(
                  min_height = 400,
                  p("Please select a district on the map below.", style = "font-size:14px"),
                  p("Note: Ganges river dolphins occur only in those districts highlighted with blue border", style = "font-size:12px; margin-bottom: 0px; margin-top: 0px;"),
                  leafletOutput("mymap", height = 250),
                  layout_columns(
                  textInput(inputId = "selecteddistrict", label = "Selected district:", value = ""),
                  selectizeInput(inputId = "river",
                                 label = "Select river(s):",
                                 choices = character(0),
                                 multiple = T))
                  )),
              layout_columns(
                card(
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
            DT::dataTableOutput("table")),
  nav_panel(title = "About", 
            h4(style = "font-size:15px", strong("Description of the app")),
            p(style = "font-size:14px", "The Ganges River Dolphin Population Tracker app provides information on population size and trends for all districts and river stretches included in those districts, for Bihar, where river dolphins are found. As forest divisions usually include one or more districts, 
              we believe that providing such information can provide critical support for conservation planning, management, threat mitigation, and action."), 
            p(style = "font-size:14px", "We have regularly seen that river survey reports for species such as Ganges river dolphins do not provide information on district-wise population estimates. 
              Usually estimates or counts are provided for the entire river stretch or for stretches between specific reference points such as bridges or ghats. While this information is useful as general knowledge, 
              its utility becomes limited for divisional forest offices. If such data are made available at the district or division level, monitoring of trends and impact of conservation measures taken for 
              the habitat and populations can be more impactful and possible within the administrative jurisdiction of divisional offices. This app addresses this important gap."),
            p(style = "font-size:14px", "The app incorporates population data for all districts in the rivers of Bihar where Ganges river dolphins are found. It has been developed by the Riverine Ecosystems and Livelihoods programme, 
              Wildlife Conservation Trust, who have been monitoring Ganges river dolphins in all rivers of Bihar for nearly two decades. These data are intended to support the Department of Environment, Forests, and Climate Change, 
              Government of Bihar, and all divisional forest officials in the conservation and management of Ganges river dolphin populations and river habitats in Bihar."),
            p(style = "font-size:14px", "In the app, users can select a district by clicking it on the map view and select rivers with stretches falling within that district, to explore trends in the number of dolphins recorded per km of river) as well as 
              the latest available population estimates for that stretch. The app also provides access to the Ganges river dolphin data compiled by us in the 'Database' tab. 
              Users can also view the complete dataset when no district or river is selected, or access specific data for the selected district and rivers. The dataset includes information on survey year and month (last available), 
              estimated or minimum population size (with range), encounter rate (number of dolphins per km of river), the error associated with estimates 
              of population size per km (standard deviation or SD per km), dolphin population status (resident, seasonal visitor, small or extirpated), the survey method used, and the data sources and references."),
            h4(style = "font-size:15px", strong("Interpretation of data according to survey method")),
            p(style = "font-size:14px", "Boat-based surveys are the best methods to carry out river dolphin population estimation. Interview surveys, shore surveys, and opportunistic reports of dolphin presence should only be considered when any of the following conditions prevail."), 
              tags$ol(
              style = "font-size:14px",
              tags$li("Rivers are not navigable."),
              tags$li("Very few dolphins are likely to be present."),
              tags$li("There is no possibility of conducting boat-based surveys due to lack of resources or capacity.")
              ),
           p(style = "font-size:14px", "If single-observer survey methods are used, upstream surveys are preferable because boat speeds are slower (boat moves against river flow direction) and 
              there is more time to detect surfacing dolphins. Downstream surveys (in the direction of river flow), in case of single-observer surveys, may tend to miss surfacing dolphins more than upstream surveys. 
              Estimates from single-observer survey methods are usually minimum counts and hence underestimates of the true population. However, if sighting distance and bearing data are available, it is possible to estimate population size closer to the true population using statistical analyses."),
           p(style = "font-size:14px", "Double-observer survey methods allow for estimation of uncertainty in population size, that may result from the two sources of bias listed below."),
           tags$ol(
             style = "font-size:14px",
             tags$li("Detection bias: not seeing all dolphins that surface, due to sighting conditions, observer attention, human error, etc."),
             tags$li("Availability bias: not seeing dolphins because they did not surface at the time of boat passage.")
             ),
           p(style = "font-size:14px", "If double-observer surveys are conducted in the downstream direction, dolphins missed by one team may still be detected by another team, making the surveys highly efficient as well as reasonably accurate in estimation of population size."),
           p(style = "font-size:14px", "The table below provides a quick guide to interpret river dolphin population estimates reported based on different survey methods."),
           shiny::tableOutput("surveymethod_table"), ##for rendering static tables
           h4(style = "font-size:15px", strong("IUCN Red List Assessment for Ganges river dolphins")),
           p(style = "font-size:14px", "Some of the data provided here has been extracted from the database on Ganges river dolphin population sizes for different rivers compiled by Kelkar et al. (2022) for the IUCN Red List Assessment for Ganges river dolphins."),
           h4(style = "font-size:15px", strong("Other data sources")),
           p(style = "font-size:14px", "Wherever information from other reports and papers (not involving WCT-REAL programme) has been used or gleaned, or interpreted, the sources have been duly cited and acknowledged."),
           h4(style = "font-size:15px", strong("References")),
           p(style = "font-size:14px", "Ashoka Trust for Research in Ecology and the Environment (ATREE). (2018). Report on population status of Gharials and Ganges river dolphins in the Gandak River, November 2017. Submitted to the Chief Wildlife Warden, Department of Environment, Forests, and Climate Change, Govt. of Bihar."),
           p(style = "font-size:14px", "Choudhary, S., Dey, S., Sagar, V., Nair, T. & Kelkar, N. (2012). River dolphin distribution in regulated river systems: implications for dry-season flow regimes in the Gangetic basin. Aquatic Conservation: Marine and Freshwater Ecosystems 22: 11-25."),
           p(style = "font-size:14px", "Choudhary, S.K. (2019a). Status of Ganges River dolphins in Kosi river in Supaul district, Bihar, with notes on other riverine biodiversity – January 2019. Final report submitted to the Divisional Forest Officer, Supaul Forest Division, Department of Environment, Forests, and Climate Change, Bihar, India, 29 p."),
           p(style = "font-size:14px", "Choudhary, S.K. (2019b). Status of Ganges River dolphins in Kosi river in Saharsa district, Bihar, with notes on other riverine biodiversity – 2019. Final report submitted to the Divisional Forest Officer, Saharsa Forest Division, Department of Environment, Forests, and Climate Change, Bihar, India, 40 p."),
           p(style = "font-size:14px", "Choudhary, S.K. (2019c). Status of Ganges River dolphins in the five Rivers Parman, Bakra, Kankai, Mechi and Mahananda within Araria Forest Division, Bihar, with notes on other riverine biodiversity – 2019. Final report submitted to the Divisional Forest Officer, Araria Forest Division, Department of Environment, Forests, and Climate Change, Bihar, India, 55 p."),
           p(style = "font-size:14px", "Choudhary, S.K. & Dey, S. (2020). Status of Ganges river dolphins in the Ganga River (Simariya Ghat, Mokama to Manihari) in Bihar, India. Report on Bihar Dolphin Survey submitted to the Chief Wildlife Warden, Department of Environment, Forests, and Climate Change, Bihar, India."),
           p(style = "font-size:14px", "Das, G. C., Usmani, A. A., Sharma, S. P., Guha, S., Ali, S. Z., Barthwal, S., ... & Hussain, S. A. (2024). Conservation planning for Gangetic dolphin (Platanista gangetica) in smaller rivers of the Ganga River Basin, India. Global Ecology and Conservation, 51, e02900."),
           p(style = "font-size:14px", "Kelkar, N. & Dey, S. (2021). Ganges river dolphins and other biodiversity in the Mahananda River in Bihar and West Bengal: A report on the first complete survey, November 2021. Report for the Department of Environment, Forests, and Climate Change, Government of Bihar. Wildlife Conservation Trust, Mumbai, India, 35 pp."),
           p(style = "font-size:14px", "Kelkar, N. (2015). Strengthening the meaning of a freshwater protected area for the Ganges River Dolphin: looking within and beyond the Vikramshila Gangetic Dolphin Sanctuary, Bihar, India. Final report submitted to the Small Cetacean Fund, International Whaling Commission (IWC), United Kingdom, 45 p."),
           p(style = "font-size:14px", "Kelkar, N. & Dey, S. (2022). Expanding long-term monitoring of Ganges river dolphins and fisheries in the Vikramshila Gangetic Dolphin Sanctuary: Technical Report on the work conducted from January to April 2022. Submitted to the Department of Environment, Forests, and Climate Change, Patna and Bhagalpur, Government of Bihar. iv + 32 p."),
           p(style = "font-size:14px", "Kelkar, N., Bakshi, S. & Dey, S. (2023a). Population size, apparent trends, and threats to Ganges River Dolphins in the Ganga River, Sahibganj district, Jharkhand (2014-2023). Technical Report submitted to the PCCF-WL and CWLW, & DFO-Sahibganj Division, Jharkhand Forest Department. Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust. 14 p."),
           p(style = "font-size:14px", "Kelkar, N., Dey, S., Ramya Roopa, .S., Dey, S., Kumar, A., Kumar, R., Das, K.K. & Bakshi, S. (2023b). Ganges River Dolphins in the Vikramshila Gangetic Dolphin Sanctuary, Bhagalpur, Bihar. Status report on latest estimation of population size: February 2023. Technical report submitted by Wildlife Conservation Trust to the Divisional Forest Officer, Bhagalpur Division, Dept. of Environment, Forests, and Climate Change, Government of Bihar, 5 p."),
           p(style = "font-size:14px", "Kelkar, N., Smith, B.D., Alom, M.Z., Dey, S., Paudel, S. & Braulik, G.T. (2022). Platanista gangetica. The IUCN Red List of Threatened Species 2022: e.T41756A50383346.", 
           tags$a(href = "https://dx.doi.org/10.2305/IUCN.UK.2022-1.RLTS.T41756A50383346.en", "https://dx.doi.org/10.2305/IUCN.UK.2022-1.RLTS.T41756A50383346.en", target = "_blank")), 
           p(style = "font-size:14px", "Khanal, G. & Kelkar, N. (2023) Understanding the effects of trans-boundary barrage operations on the Nepal-India border for Ganges river dolphin habitat and population dynamics. Final Report submitted to the IWC Small Cetacean Fund, December 2023. 44 p."),
           p(style = "font-size:14px", "Qureshi, Q., Hussain, S.A., Kolipakam, V., Wakid, A., Raza, R., Choudhury, S.K., Talukdar, V., Ray, S., Singh, V., Deori, S., Rastogi, R., Warudkar, A., Goyal, N., Sharma, S., Jacob, M., Roy, K. & Chowdhury, G. R. (2018). Development of Conservation Action Plan for Ganges River Dolphin. Annual Report 2017-18. Wildlife Institute of India, Dehradun."),
           p(style = "font-size:14px", "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust (REAL-WCT). (2022). Assessing population and habitat changes for Ganges river dolphins and Gharials in the regulated Gandak River, India: conservation and water management implications. Report submitted to the APCCF-cum-Chief Wildlife Warden, Department of Environment, Forests and Climate Change, Government of Bihar. iii + 20 p."),
           p(style = "font-size:14px", "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust (REAL-WCT). (2023a). Ganges river dolphin population abundance, distribution, and threats in the Kosi River, Bihar – February 2023. Technical report submitted to the APCCF-cum-CWLW, Dept. of Environment, Forests, and Climate Change, Govt. of Bihar, iii + 10 p."),
           p(style = "font-size:14px", "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust (REAL-WCT). (2023b). Prospects for conservation of threatened wildlife along the Ghaghra River, India: a technical report. Report submitted to the APCCF-cum-Chief Wildlife Warden, Dept. of Environment, Forests, and Climate Change, Govt. of Bihar, 28 p."),
           p(style = "font-size:14px", "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust (REAL-WCT). (2024a). Research and Monitoring Activities in the Vikramshila Gangetic Dolphin Sanctuary, Bhagalpur, Bihar: May 2023 to March 2024. Technical report submitted by Wildlife Conservation Trust to the APCCF-cum-Chief Wildlife Warden and Divisional Forest Officer, Bhagalpur Division, Dept. of Environment, Forests, and Climate Change, Government of Bihar, 43 p."),
           p(style = "font-size:14px", "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust (REAL-WCT). (2024b). Ganges river dolphin population size, distribution, and threats in the Bagmati River (Samastipur and Khagaria districts), Bihar – December 2023. Technical report submitted to the APCCF-cum-CWLW, Dept. of Environment, Forests, and Climate Change, Govt. of Bihar, iii + 6 p."),
           p(style = "font-size:14px", "Sen, A., Das, D. & Chatterjee, A., (2016). Technique adopted to rescue and rehabilitate Ganges river dolphin, Platanista gangetica (Roxburg,1801), from Donk River at Kishan District of Bihar, India. International Research Journal of Natural and Applied Sciences 3(8): 175–185."), 
           p(style = "font-size:14px", "Sharma, G. (2019). The Ganges River Dolphin in the Rivers of Bihar. Report submitted to Department of Environment, Forest and Climate Change. Govt. of Bihar India, 45 p."),
           p(style = "font-size:14px", "Sharma, G. (2013). Current status of susu (Platanista gangetica gangetica, Roxburgh, 1801) in River Gandak, a major tributary of the holy River Ganga in Bihar, India. Records of the Zoological Survey of India 113: 39-59."),
           p(style = "font-size:14px", "Sinha, R.K. & Sharma, G. (2003). Current status of the Ganges river dolphin in the rivers Kosi and Son. Journal of the Bombay Natural History Society 100(1): 27-37."),
           p(style = "font-size:14px", "Sinha, R.K., Smith, B.D., Sharma, G., Prasad, K., Choudhury, B.C., Sapkota, K., Sharma, R.K. & Behera, S.K. (2000). Status and distribution of the Ganges Susu, Platanista gangetica, in the Ganges River system of India and Nepal. In: R.R. Reeves, B.D. Smith, and T. Kasuya (eds) Biology and Conservation of Freshwater Cetaceans in Asia, pp. 54–61. IUCN Species Survival Commission Occasional Paper No. 23, Gland, Switzerland and Cambridge, UK."),
           tags$hr(style = "border-color:#d3d3d3; background-color: #333; height: 2px"),
           h4(style = "font-size:15px", strong("Funding Support to REAL programme, Wildlife Conservation Trust")),
           p(style = "font-size:14px", "BNP Paribas Foundation, Duleep Matthai Nature Conservation Trust, International Whaling Commission Small Cetacean Fund, Rohini Nilekani Philanthropies Foundation, DSP HMK Holdings Pvt. Ltd., Dolphin Quest."),
           div(img(src = "BNPP.png", alt = "BNP Paribas Foundation", height = "80px", width = "auto"),
               img(src = "DMNCT.png", alt = "Duleep Matthai Nature Conservation Trust", height = "80px", width = "auto"),
               img(src = "IWC.gif", alt = "International Whaling Commission Small Cetacean Fund", height = "70px", width = "auto"),
               img(src = "RNP.png", alt = "Rohini Nilekani Philanthropies Foundation", height = "70px", width = "auto"),
               img(src = "DSP-HMK.png", alt = "DSP HMK Holdings Pvt. Ltd.", height = "70px", width = "auto"),
               img(src = "DQ.jpg", alt = "Dolphin Quest", height = "70px", width = "auto")),
           h4(style = "font-size:15px", strong("Developed by")),
           p(style = "font-size:14px", "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust."),
           h4(style = "font-size:15px", strong("Credits")),
           tags$ul(
             style = "font-size:14px",
             tags$li(strong("Conceptualization and data compilation:"), "Dr. Nachiket Kelkar"),
             tags$li(strong("Data collection, curation, and compilation:"), "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust"),
             tags$li(strong("App development:"), "S. Ramya Roopa")
           ),
           h4(style = "font-size:15px", strong("Code Availability")),
           p(style = "font-size:14px", "The", tags$i(class = "fab fa-r-project", title = "R Project"), "code for this app is available in",  tags$a(href = "https://github.com/WCT-Riverine-Ecology-And-Livelihoods/GangesRiverDolphin-Pop-Tracker.git", "this", target = "_blank"), tags$i(class = "fa-brands fa-github", title="Github"), "repository."),
           tags$br()
  )
)

##Server
server <- function(input, output, session) {
  data <- reactiveValues(clickedShape = NULL)
  ##Output - Basic map of Bihar
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 86, lat = 26,  zoom = 5.5) %>%
      addPolygons(data = bihar_simple,
                  weight = 1.5,
                  color = ~GRD_presence_pal(GRD_presence),
                  fillColor = ~GRD_presence_pal(GRD_presence),
                  fillOpacity = 0.3,
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
                    color = ~GRD_presence_pal(GRD_presence),
                    fillColor = ~GRD_presence_pal(GRD_presence),
                    fillOpacity = 0.3,
                    layerId = ~DISTRICT,
                    label = ~paste("District:" , DISTRICT) %>% lapply(htmltools::HTML)) %>%
          addPolygons(data = selected_district_df(),
                      color = "#3459e6",
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
      title = NULL,
      class = "nopad",
      value = tags$p(paste(paste0(unlist(input$river)[1], ","), df_river1()$year[1], ":", 
                           df_river1()$pop_estimate[1], "±", round(df_river1()$sd[1], 0), "dolphins"), 
                     style = "font-size: 100%;"),
      theme = "primary", 
      max_height = "80px"
    ), 
    value_box(
      title = NULL,
      class = "nopad",
      value = tags$p(paste("River length", ":", df_river1()$km[1], "km"), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    ),
    value_box(
      title = NULL,
      class = "nopad",
      value = tags$p(paste(paste0(unlist(input$river)[2], ","), df_river2()$year[1], ":", 
                           df_river2()$pop_estimate[1], "±", round(df_river2()$sd[1], 0), "dolphins"), 
                     style = "font-size: 100%;"),
      theme = "primary",
      max_height = "80px"
    ), 
    value_box(
      title = NULL,
      class = "nopad",
      value = tags$p(paste("River length", ":", df_river2()$km[1], "km"), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    ),
    value_box(
      title = NULL,
      class = "nopad",
      value = tags$p(paste(paste0(unlist(input$river)[3], ","), df_river3()$year[1], ":", 
                           df_river3()$pop_estimate[1], "±", round(df_river3()$sd[1],0), "dolphins"), 
                     style = "font-size: 100%;"),
      theme = "primary",
      max_height = "80px"
    ), 
    value_box(
      title = NULL,
      class = "nopad",
      value = tags$p(paste("River length", ":", df_river3()$km[1], "km"), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    ),
    value_box(
      title = NULL,
      class = "nopad",
      value = tags$p(paste(paste0(unlist(input$river)[4], ","), df_river4()$year[1], ":", 
                           df_river4()$pop_estimate[1], "±", round(df_river4()$sd[1],0), "dolphins"), 
                     style = "font-size: 100%;"),
      theme = "primary",
      max_height = "80px"
    ), 
    value_box(
      title = NULL,
      class = "nopad",
      value = tags$p(paste("River length", ":", df_river4()$km[1], "km"), style = "font-size: 100%;"),
      theme = "secondary",
      max_height = "80px"
    )
    ))
    
    ##Output - Graph
    output$graph <- renderPlotly({
        req(input$river)
        graph_df <- selected_river_table() %>% arrange(date)
        x_axis_breaks  <- seq(
          from = min(graph_df$date) %m-% years(1), 
          to = max(graph_df$date) %m+% years(1),
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
                   hovertemplate = paste('Survey date: %{x|%b-%Y}',
                                         '<br>Encounter rate: %{y}<extra></extra>')) %>%
           layout(
            yaxis = list(title = "Encounter rate <br> (no. of dolphins/km)"),
            xaxis = list(
                         autorange = F,
                         range = c(min(graph_df$date) %m-% years(1), max(graph_df$date) %m+% years(1)),
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
        layout_column_wrap(
          width = 1/2, ##width = 1/n where n is the number of columns in the grid
          vbs()[[1]], vbs()[[2]])
      }
      else if(length(unlist(input$river)) == 2){
        layout_column_wrap(
          width = 1/2,
          vbs()[[1]], vbs()[[2]], vbs()[[3]], vbs()[[4]])
      } 
      else if(length(unlist(input$river)) == 3){
        layout_column_wrap(
        width = 1/2,
        vbs()[[1]], vbs()[[2]], vbs()[[3]], 
        vbs()[[4]], vbs()[[5]], vbs()[[6]])
      } else if(length(unlist(input$river)) == 4){
        layout_column_wrap(
          width = 1/2,
          vbs()[[1]], vbs()[[2]], vbs()[[3]], vbs()[[4]], 
          vbs()[[5]], vbs()[[6]], vbs()[[7]], vbs()[[8]])
      }
      })
    
    output$table <- DT::renderDataTable({ ##use isTruthy to find out if value is truthy i.e it is not FALSE, NULL, "", or an empty vector
      if(is.null(input$selecteddistrict) == FALSE & isTruthy(input$river)){
        dt_table %>% filter(District %in% unlist(input$selecteddistrict)) %>% arrange(River, Year)
      } 
      else if(isTruthy(input$selecteddistrict) & isTruthy(input$river)){
        dt_table %>% filter(District %in% unlist(input$selecteddistrict)) %>%
                     filter(River %in% unlist(input$river)) %>% arrange(River, Year)
        }
      else {
        dt_table %>% arrange(District, River, Year)
      } 
    })
    
    output$surveymethod_table <- shiny::renderTable({surveymethod_table}, 
                                                    bordered = TRUE)  
}

shinyApp(ui, server)