library(shiny)
library(leaflet)

shinyUI(navbarPage("AirSent",

  tabPanel("Map",
     # Sidebar with a slider input for number of bins
     sidebarLayout(
       sidebarPanel(
         selectInput("airline_select_main", label = h3("Airline"), 
                     choices = airlines, 
                     selected = 1),
         htmlOutput("stats")
       ),
       
       # Show a plot of the generated distribution
       mainPanel(
         #lotOutput("distPlot")
         leafletOutput("map")
       )
     ),
     
     dataTableOutput("stateStats")
  ),
  tabPanel("Sample Data",
   dataTableOutput("sampleData")
  ),
  tabPanel("Real Time", 
   sidebarLayout(
     sidebarPanel(
       selectInput("airline_select_realtime", label = h3("Airline"), 
                   choices = airlines, 
                   selected = 1),
       sliderInput("tweet_number", label = h3("Number of Tweets"),
                   min = 10, max = 100, value = 50),
       actionButton("submit", "GO")
       #p("Only classifications with a confidence greater than 0.9 are shown")
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       leafletOutput("realtime_map")
     )
   ),
   dataTableOutput("realtimeData")
  )
))
