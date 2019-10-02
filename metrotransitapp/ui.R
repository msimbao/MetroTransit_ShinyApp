library(shinydashboard)
library(leaflet)
library(readr)
library(ggvis)
library(shinythemes)

metro_gas <- read_csv("metro_gas.csv")
trips  <- readRDS("trips.rds")
shapes <- readRDS("shapes.rds")


header <- dashboardHeader(
  title = "Twin Cities Routes"
)

body <- dashboardBody(skin = "green",
  fluidRow(
    column(width = 6,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("busmap", height = 500)
           )
    ),
    
    column(width = 6,
           box(width = NULL, status = "warning",
               uiOutput("routeSelect")
           ),
           box( width = NULL,
                ggvisOutput("cocaine_state")
                
    )
  )

)
)

fluidPage(
  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    "))
    ),
  
dashboardPage(skin="red",
  header,
  dashboardSidebar(disable = TRUE),
  body
)
)