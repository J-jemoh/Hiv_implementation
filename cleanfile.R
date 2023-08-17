library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(shiny)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)
library(readxl)
library(dplyr)
library(sf)

# Load your CombinedDataSet CSV
hivdata <- read.csv("CombinedDataSet.csv")
usecases<-read.csv("UseCases.csv",skip = 1,header = TRUE)
ehecoutcmes<-read_excel("UseCases.xlsx",skip = 2,sheet=2,col_names = TRUE)
combinedDataV1<-read_excel("CombinedDataSet_vl.xls",col_names = TRUE)

barriers<-read_excel("UseCases.xlsx",skip = 2,sheet=3,col_names = TRUE)
communityPartners<-read_excel("UseCases.xlsx",skip = 2,sheet=4,col_names = TRUE)
ehe_countyinfo<-read_excel("EHE_CountyInfo.xlsx",col_names =TRUE)
study_designs<-read_excel("StudyDesignsandDataCollection.xls",col_names = TRUE)
combinedData<-merge(study_designs,ehe_countyinfo,by="StudyID")
combinedData<-combinedData %>%
    distinct(StudyDesign, .keep_all = TRUE)
write.csv(combinedData,file = "CombinedData.csv")
# ehe_countyinfo_unique <- ehe_countyinfo %>%
#     distinct(StudyID, .keep_all = TRUE)
# 


names(combinedDataV1)[names(combinedDataV1)=="State"]<-"name"
combinedv1_merged<-merge(us_states,combinedDataV1,by="name",all = TRUE)
combinedv1_merged<-subset(combinedv1_merged,StudyDesign!="" & StudyID!="")
# Assuming your_data is your dataframe
combinedv1_merged <- combinedv1_merged[!(combinedv1_merged$StudyID == 9 & seq_along(combinedv1_merged$StudyID) > (length(combinedv1_merged$StudyID) - 6)), ]
write.csv(combinedv1_merged,file = "mergedDataset.csv",row.names = FALSE)
saveRDS(combinedv1_merged,file="mergedDataset.RDS")


#check using tms
word_borders<-st_read("geo/TM_WORLD_BORDERS_SIMPL-0.3.shp")

us_states <- st_read("geojson/ne_10m_admin_1_states_provinces.shp")
print(names(us_states))

mypalette <- colorNumeric( palette="viridis", domain=us_states$county, na.color="transparent")


# study_designs_unique <- study_designs %>%
#     distinct(StudyID, .keep_all = TRUE)
locations <- paste(combinedData$State, combinedData$County, sep = ", ")

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

# Add latitude and longitude columns to the dataset
api_key <- "AIzaSyCVSSQStpsE-NC3nskIbt0OqCYvQg8KPU8"

hivdata_with_coords <- combinedData %>%
    mutate(coords = map(locations, ~get_coordinates(.x, api_key))) %>%
    mutate(lat = sapply(coords, function(x) if (!is.null(x)) x["lat"] else NA),
           lng = sapply(coords, function(x) if (!is.null(x)) x["lng"] else NA)) %>%
    select(-coords)  # Remove the temporary 'coords' column

# Save the updated dataset with latitude and longitude columns
write.csv(hivdata_with_coords, "CombinedDataSet_WithCoords.csv", row.names = FALSE)

names(hivdata_with_coords)[names(hivdata_with_coords)=="Pillar(s)"]<-"Pillars"
names(hivdata_with_coords)[names(hivdata_with_coords)=="Community Partner"]<-"community_partner"
write.csv(hivdata_with_coords, "CombinedDataSet_WithCoords.csv", row.names = FALSE)