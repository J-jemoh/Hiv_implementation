library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(shiny)
library(leaflet)
library(shinydashboard)

# Load your CombinedDataSet CSV
hivdata <- read.csv("CombinedDataSet.csv")
api_key <- "#"  # Replace with your actual API key

# Extract State and EHEGeographicPriorityArea columns from the dataset
locations <- paste(hivdata$State, hivdata$EHEGeographicPriorityArea, sep = ", ")

# Define a function to get coordinates using Google Maps Geocoding API
get_coordinates <- function(location, api_key) {
    url <- sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s",
                   URLencode(location), api_key)
    
    response <- GET(url)
    data <- content(response, as = "parsed")
    
    if (data$status == "OK") {
        lat <- data$results[[1]]$geometry$location$lat
        lng <- data$results[[1]]$geometry$location$lng
        return(c(lat = lat, lng = lng))
    } else {
        return(NULL)
    }
}

# Define UI for Shiny app
# ui <- fluidPage(
#     titlePanel("HIV Implementation Data Map"),
#     sidebarLayout(
#         sidebarPanel(
#             # Input for Google Maps API Key
#             textInput("api_key", "Enter your Google Maps API Key:", value = ""),
#             # Button to generate map
#             actionButton("generate_map", "Generate Map")
#         ),
#         mainPanel(
#             leafletOutput("map")
#         )
#     )
# )
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
        title = "HIV Implementation Data Map"
    ),
    dashboardSidebar(
        sidebarMenu(
            style = "background-color:#3e5c74;",
            menuItem(
                text = "Implementation map",
                tabName = "map_chart"
                # textInput("api_key", "Enter API Key:", placeholder = "Enter your Google Maps API Key"),
                # actionButton("generate_map", "Generate Map")
            ),
            menuItem(
                text = "Publications",
                tabName = "map_data"
                # textInput("api_key", "Enter API Key:", placeholder = "Enter your Google Maps API Key"),
                # actionButton("generate_map", "Generate Map")
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("map_chart",
                    fluidRow(
                        box(width=12, title="HIV Implementation Data Map",
                            # textInput("api_key", "Enter Google Maps API Key:", ""),
                            # actionButton("generate_map", "Generate Map"),
                           
                            leafletOutput("map",height = "600px"),  # Display the map here
                            status = "primary"
                        )
                    )
            ),
            tabItem("map_data",
                    fluidRow(
                        box(width=12, title="HIV Implementation Data ",
                
                            dataTableOutput("data_map"),style = "max-width: 100%; overflow-x: auto;",  # Display the table here
                            status = "primary"
                        )
                    )
            )
        )
    )
)

# Define server logic for Shiny app
server <- function(input, output, session) {
    
    observeEvent(TRUE, {
        # api_key <- input$api_key
        
        if (!is.null(api_key) && nchar(api_key) > 0) {
            hivdata_with_coords <- hivdata %>%
                mutate(coords = map(locations, ~get_coordinates(.x, api_key))) %>%
                mutate(lat = sapply(coords, function(x) if (!is.null(x)) x["lat"] else NA),
                       lng = sapply(coords, function(x) if (!is.null(x)) x["lng"] else NA)) %>%
                select(-coords)  # Remove the temporary 'coords' column
            
            output$map <- renderLeaflet({
                leaflet() %>%
                    addTiles() %>%
                    addMarkers(data = hivdata_with_coords,
                               lat = ~lat,
                               lng = ~lng,
                               label = ~State,
                               clusterOptions = markerClusterOptions(),
                               popup = paste("<strong>State:</strong> ", hivdata_with_coords$State, "<br>",
                                             "<strong>Priority Area:</strong> ", hivdata_with_coords$EHEGeographicPriorityArea,"<br>",
                                             "<strong>ProjectTitle: </strong>",hivdata_with_coords$ProjectTitle,"<br>",
                                             "<strong>YearofEHEAward:</strong> ",hivdata_with_coords$YearofEHEAward,"<br>",
                                             "<strong>CascadeTarget:</strong>",hivdata_with_coords$CascadeTarget),
                               options = markerOptions(autoPan = FALSE))
            })
        }
    })
    
    output$data_map<-renderDataTable({
        columns<-c("StudyID","CascadeTarget","DesignType","ProjectTitle","ImplementingPartner","YearofEHEAward")
        hivdata[columns]
        
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
