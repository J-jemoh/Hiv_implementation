library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(shiny)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)
library(readxl)
library(sf)

# Load your CombinedDataSet CSV
hivdata <- read.csv("CombinedDataSet.csv")
usecases<-read.csv("UseCases.csv",skip = 1,header = TRUE)
ehecoutcmes<-read_excel("UseCases.xlsx",skip = 2,sheet=2,col_names = TRUE)
barriers<-read_excel("UseCases.xlsx",skip = 2,sheet=3,col_names = TRUE)
communityPartners<-read_excel("UseCases.xlsx",skip = 2,sheet=4,col_names = TRUE)
ehe_countyinfo<-read_excel("EHE_CountyInfo.xlsx",col_names =TRUE)
study_designs<-read_excel("StudyDesignsandDataCollection.xls",col_names = TRUE)
combinedData<-read.csv("CombinedData.csv")
combined_with_codes<-read.csv("CombinedDataSet_WithCoords.csv")
mergedDataset<-readRDS("mergedDataset.RDS")


us_states <- st_read("geojson/ne_10m_admin_1_states_provinces.shp")

# mypalette <- colorNumeric( palette="viridis", domain=us_states$county, na.color="transparent")
mypalette <- colorNumeric(palette = "viridis", domain = mergedDataset$gn_id,na.color="transparent")
mypalette(c(45,43))

api_key <- "AIzaSyCVSSQStpsE-NC3nskIbt0OqCYvQg8KPU8"  # Replace with your actual API key

# # Extract State and EHEGeographicPriorityArea columns from the dataset
# locations <- paste(combinedData$State, combinedData$County, sep = ", ")
mylabels<- paste("<strong>Cascade Target:</strong> ", mergedDataset$CascadeTarget, "<br>",
                 "<strong>Community Partner:</strong> ", mergedDataset$CommunityPartner,"<br>",
                 "<strong>Implementing Partner: </strong>",mergedDataset$ImplementingPartner,"<br>",
                 "<strong>EHE Priority Area:</strong> ",mergedDataset$EHEGeographicPriorityArea,"<br>",sep = "")%>%
                    lapply(htmltools::HTML)


# Define UI for Shiny app

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
 
            ),
            menuItem(
                text = "Use Cases",
                tabName = "map_data"
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
                        mainPanel(style="width:100%;",
                            tabsetPanel(
                                tabPanel(h3(strong("Published")),
                                         box(width=12, title = strong("Published"),tableOutput("published"))
                                         ),
                                tabPanel(h3(strong("Outcomes")),
                                         box(width=12, title = strong("Outcomes"),tableOutput("outcomes"))
                                ),
                                tabPanel(h3(strong("Barriers and Facilitators")),
                                         box(width=12, title = strong("Barriers and Facilitators"),tableOutput("barriers"))
                                ),
                                tabPanel(h3(strong("Community Involvement")),
                                         box(width=12, title = strong("Community Involvement"),tableOutput("cinvolement"))
                                ),
                            )
                        )
                    ),
                    # fluidRow(
                    #     box(width=12, title="HIV Implementation Data ",
                    #
                    #         tableOutput("data_map"),style = "max-width: 100%; overflow-x: auto;",  # Display the table here
                    #         status = "primary"
                    #     )
                    # )
            )
        )
    )
)

# Define server logic for Shiny app
server <- function(input, output, session) {

    # observeEvent(TRUE, {
    #     # api_key <- input$api_key
    # 
    #     if (!is.null(api_key) && nchar(api_key) > 0) {
            # hivdata_with_coords <- combinedData %>%
            #     mutate(coords = map(locations, ~get_coordinates(.x, api_key))) %>%
            #     mutate(lat = sapply(coords, function(x) if (!is.null(x)) x["lat"] else NA),
            #            lng = sapply(coords, function(x) if (!is.null(x)) x["lng"] else NA)) %>%
            #     select(-coords)  # Remove the temporary 'coords' column

            output$map <- renderLeaflet({
                leaflet() %>%
                    addTiles() %>%
                    setView(lng = -95.7129, lat = 37.0902, zoom = 4) %>% 
                    
                    addPolygons(data = mergedDataset, 
                                fillColor = ~mypalette(mergedDataset$gn_id) , # Modify this based on your data
                                fillOpacity = 0.5,    # Adjust opacity
                                color = "white", 
                                weight = 1,
                                label = mylabels,
                                labelOptions = labelOptions( 
                                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                                    textsize = "13px", 
                                    direction = "auto")) 
        #     })
        # }
    })

    output$data_map<-renderTable({
        combinedData


    })
    custom_colnames<-c("S/NO", "ARTICLE TITLE", "AUTHOR","JOURNAL TITLE")
    colnames(usecases) <- custom_colnames
    output$published<-renderTable({
        usecases

    })
    custom_colnames<-c("CASCADE TARGET", "IS FRAMEWORK")
    colnames(ehecoutcmes) <- custom_colnames
    output$outcomes<-renderTable({
        ehecoutcmes

    })
    # custom_colnames<-c("CASCADE TARGET", "IS FRAMEWORK")
    # colnames(barriers) <- custom_colnames
    output$barriers<-renderTable({
        barriers

    })
    custom_colnames<-c("COMMUNITY PARTNERS", "IMPLEMENTING PARTNERS")
    colnames(communityPartners) <- custom_colnames
    output$cinvolement<-renderTable({
        communityPartners

    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
