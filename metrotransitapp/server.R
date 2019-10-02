library(shinydashboard)
library(acs)
library(leaflet)
library(dplyr)
library(readr)
library(curl) # make the jsonlite suggested dependency explicit
library(tigris)
library(htmltools)

# 1=South, 2=East, 3=West, 4=North
dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")

# Download data from the Twin Cities Metro Transit API
# http://svc.metrotransit.org/NexTrip/help
getMetroData <- function(path) {
  url <- paste0("http://svc.metrotransit.org/NexTrip/", path, "?format=json")
  jsonlite::fromJSON(url)
}

# Load static trip and shape data
metro_gas <- read_csv("metro_gas.csv")
trips  <- readRDS("trips.rds")
shapes <- readRDS("shapes.rds")
houses <- read_csv("garage.csv")
stops <- read_csv("bus-stops-Oct-2017.csv")
mini <- read_csv("acs.csv")


census_tracts <- tracts(state = "MN", county = c("Anoka County","Carver County","Dakota County","Chisago County","Hennepin County","Ramsey County","Washington County","Sibley County","Scott County","Sherburne County","Wright County","Pierce County"," Mille Lacs County","Isanti County","Le Sueur County","St. Croix County"))

data_geo <- geo_join(census_tracts, mini, by_sp = "GEOID", by_df = "GEO.id2", how = "inner")

# Get the shape for a particular route. This isn't perfect. Each route has a
# large number of different trips, and each trip can have a different shape.
# This function simply returns the most commonly-used shape across all trips for
# a particular route.
get_route_shape <- function(route) {
  routeid <- paste0(route, "-75")
  
  # For this route, get all the shape_ids listed in trips, and a count of how
  # many times each shape is used. We'll just pick the most commonly-used shape.
  shape_counts <- trips %>%
    filter(route_id == routeid) %>%
    group_by(shape_id) %>%
    summarise(n = n()) %>%
    arrange(-n)
  
  shapeid <- shape_counts$shape_id[1]
  
  # Get the coordinates for the shape_id
  shapes %>% filter(shape_id == shapeid)
}


function(input, output, session) {
  
  # Route select input box
  output$routeSelect <- renderUI({
    live_vehicles <- getMetroData("VehicleLocations/0")
    
    routeNums <- sort(unique(as.numeric(live_vehicles$Route)))
    # Add names, so that we can add all=0
    names(routeNums) <- routeNums
    routeNums <- c(All = 0, routeNums)
    selectInput("routeNum", "Route", choices = routeNums, selected = routeNums[2])
  })
  
  bus <- makeIcon(
    iconUrl = "bus.png",
    iconWidth = 30, iconHeight = 30
  )
  
  
  output$busmap <- renderLeaflet({
    
    
    data_geo$HC02_EST_VC02 <- as.numeric(data_geo$HC02_EST_VC02)
    
    data_geo$HC02_EST_VC02[is.na(data_geo$HC02_EST_VC02)] <- 0
    
    bins <- c(0, 1.18e+04,4.23e+04, 7.27e+04, 1.03e+05, 1.33e+05, 1.64e+05, 1.94e+05, Inf)
    
    pal <- colorBin("Reds", domain = data_geo$HC02_EST_VC02, bins = bins)
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Median Income<sup>2</sup>",
      data_geo$GEO.display.label, data_geo$HC02_EST_VC02
    ) %>% lapply(htmltools::HTML)
    
    
  map <-  leaflet(data = data_geo) %>% addTiles() %>%
      addTiles() %>%
      addMiniMap(position="topright") %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addMarkers(data=houses, icon=bus, ~long, ~lat, popup= ~as.character(garage),label= ~as.character(garage)) %>%
      addPolygons(data=data_geo,
                  fillColor = ~pal(HC02_EST_VC02),
                  weight = 1,
                  opacity = 0.5,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.5,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                position = "bottomright")
      
    
    if (as.numeric(input$routeNum) != 0) {
      route_shape <- get_route_shape(input$routeNum)
      
      map <- addPolylines(map,
                          route_shape$shape_pt_lon,
                          route_shape$shape_pt_lat,
                          fill = FALSE,
                          color="white",
                          smoothFactor = 3,
                          
      )
    }
    
  
    
  map
  })
  
  rct_cocaine <- reactive({
    metro_gas %>% filter((metro_gas$Route %in% input$routeNum))  
  })
  
  # works OK
  rct_cocaine %>%
    ggvis(~Price, ~Total_Riders) %>%
    layer_points(fill = ~Route, size := 50, opacity := 0.5, fill := "red") %>%
    layer_model_predictions(model="lm") %>%
    bind_shiny("cocaine_state") 
  
  
  
  
}