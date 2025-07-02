#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## Clean house & remove saved filed (include in every R script)
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(htmlwidgets)

# Modifying to share app - working as of now 
#install.packages("conflicted")
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
# Get data frame ready for Shiny app
# setwd("C:/Users/savan/OneDrive/Desktop/MATH 4802/Choropleth/overallFIRateMap")
overall_Merge <- readRDS("overall_Merge.RDS")
# View(overall_Merge)
# Define UI for application that allows user to select year to view FI rate
# Next steps: can add for other things (she did Test Rate & % Positive. Could do Child FI Rate, etc.)
# Use different colors for each RV that we do
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Food Insecurity Trends from 2009 to 2018"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Can add URL to link to overall raw data
          tags$a(href= "https://drive.google.com/drive/folders/1C45Nx2KipIX2HX9wNe5NUV_Zn1aFDlT1?usp=sharing", "2009 to 2018 MMG Data", target = "_blank"),
          # To display a note about the data
          h5("All data metrics are aggregated by year.\nNote: MMG estimates the percentage of food insecure individuals
             whose incomes fall at or below the SNAP eligibility level (130% of poverty: ≤ Low Threshold), incomes that are too high to be eligible for SNAP yet
             are within the threshold for other federal nutrition programs (between 130% and 185% of poverty: Between Thresholds), and incomes that are too high
             to be eligible for any government food assistance (above 185% FPL: > High Threshold).\n"), 
            selectInput("Year",
                        "Select a year (2009 to 2018):",
                        choices = unique(overall_Merge$year)
                        )
        ),
        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             tabPanel("Food Insecurity Rate", leafletOutput("FIRate")),
             tabPanel("# of Food Insecure Persons", leafletOutput("FICount")),
             tabPanel("% Food Insecure Persons ≤ Low Threshold", leafletOutput("FILow")),
             tabPanel("% Food Insecure Persons Between Thresholds", leafletOutput("FIBtwn")),
             tabPanel("% Food Insecure Persons > High Threshold", leafletOutput("FIHigh")),
             tabPanel("Child Food Insecurity Rate", leafletOutput("ChildFIRate")),
             tabPanel("# of Food Insecure Children", leafletOutput("ChildFICount")), 
             tabPanel("% of Children in FI HH incomes below 185% FPL", leafletOutput("ChildBelow")),
             tabPanel("% of Children in FI HH incomes above 185% FPL", leafletOutput("ChildAbove")),
             tabPanel("Cost Per Meal", leafletOutput("CPM")), 
             tabPanel("Weighted Annual Food Budget Shortfall", leafletOutput("WAD"))
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Creating a reactive function to filter data frame for selected year
  year_Data <- reactive({
    y <- overall_Merge %>% filter(year == input$Year)
    return(y)
  })
  # Building 3 tabs that each have leaflet map
  output$FIRate <- renderLeaflet({
    pal <- colorBin(palette = "OrRd", 9, domain = overall_Merge$overall_FI_Rate)
    # Calling reactive function
    labels <- sprintf(
      "</strong>%s</strong><br/>%.2f%% ",
      year_Data()$NAME, year_Data()$overall_FI_Rate * 100.0) %>%
      lapply(htmltools::HTML)
    year_Data() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      setView(lat = 34.35, lng = -82.6, zoom = 8) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(label = labels, 
                  stroke = F,
                  smoothFactor = 0.5,
                  opacity = 1, 
                  fillOpacity = 0.7, 
                  fillColor = ~pal(year_Data()$overall_FI_Rate), 
                  highlightOptions = highlightOptions(weight = 5, 
                                                      fillOpacity = 1, 
                                                      color = "black", 
                                                      opacity = 1, 
                                                      bringToFront = T)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~overall_FI_Rate,
                title = "Food Insecurity Rate", 
                opacity = 0.7)
  })
  output$FICount <- renderLeaflet({
    pal <- colorBin(palette = "YlOrRd", 9, domain = overall_Merge$overall_FI_Count)
    # Calling reactive function
    labels <- sprintf(
      "</strong>%s</strong><br/>%g Food Insecure Persons",
      year_Data()$NAME, year_Data()$overall_FI_Count) %>%
      lapply(htmltools::HTML)
    year_Data() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      setView(lat = 34.35, lng = -82.6, zoom = 8) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(label = labels, 
                  stroke = F,
                  smoothFactor = 0.5,
                  opacity = 1, 
                  fillOpacity = 0.7, 
                  fillColor = ~pal(year_Data()$overall_FI_Count), 
                  highlightOptions = highlightOptions(weight = 5, 
                                                      fillOpacity = 1, 
                                                      color = "black", 
                                                      opacity = 1, 
                                                      bringToFront = T)) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~overall_FI_Count,
                title = "# of Food Insecure Persons", 
                opacity = 0.7)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


### Choropleth for FI rate from 2009 to 2018:
# Basic summaries of FI rate
sd(overall_FI_Data$overall_FI_Rate)
summary(overall_FI_Data$overall_FI_Rate)
## Creating shapefile for 15 county region
library(plotly)
# Download to a temporary file 
temp_geoJSON <- tempfile(fileext = ".geojson")
download.file(
  "https://raw.githubusercontent.com/plotly/datasets/refs/heads/master/geojson-counties-fips.json", 
  temp_geoJSON
)
# Read the downloaded geoJSON file with sf library
library(sf)
my_Sf <- read_sf(temp_geoJSON)
# Selecting subset of only GA: State is 13
my_Sf <- my_Sf[my_Sf$STATE == 13,]
# Selecting subset of only service region: id is FIPS 
my_Sf2 <- my_Sf
View(my_Sf2)
my_Sf2 <- my_Sf2[my_Sf2$id %in% c(13281, 13241, 13311, 13137, 13257, 13011, 13119, 13147, 13195, 
                                  13157, 13105, 13013, 13059, 13221, 13219), ]
### Mapping the interactive choropleth by merging
### my_Sf2 is geospatial, overall_FI_data is the data frame
# 11/6 Video
library(tidyverse)
library(tigris)
options(tigris_use_cache = T)
library(leaflet)
library(htmlwidgets)
# Merge overall FI data with shapefile
overall_FI_Data$overall_FIPS <- as.character(overall_FI_Data$overall_FIPS)
library(dplyr)
overall_Merge <- my_Sf2 %>%
  left_join(overall_FI_Data, by = c("id" = "overall_FIPS"))
View(overall_Merge)
# Save overall_Merge dataframe as a RDS for Shiny app
saveRDS(overall_Merge, "C:/Users/savan/OneDrive/Desktop/MATH 4802/Choropleth/overallFIRateMap/overall_Merge.RDS")
## Inspecting data
# Check distribution of FI rate
library(ggplot2)
overall_Merge %>% 
  ggplot(aes(x=overall_FI_Rate)) + 
  geom_histogram(bins=12, fill = '#69b3a2', color='white')
max(overall_Merge$overall_FI_Rate) # 0.223
min(overall_Merge$overall_FI_Rate) # 0.068
## Make interactive map of overall FI rate
# Creating labels
labels <- sprintf(
  "</strong>%s</strong><br/>%g Food Insecurity Rate",
  overall_Merge$id, overall_Merge$overall_FI_Rate) %>%
  lapply(htmltools::HTML)
# Creating palette
pal <- colorBin(palette = "OrRd", 9, domain = overall_Merge$overall_FI_Rate)
#?addProviderTiles
# Creating interactive map
map_interactive <- overall_Merge %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(label = labels, 
              stroke = F,
              smoothFactor = 0.5,
              opacity = 1, 
              fillOpacity = 0.7, 
              fillColor = ~pal(overall_Merge$overall_FI_Rate), 
              highlightOptions = highlightOptions(weight = 5, 
                                                  fillOpacity = 1, 
                                                  color = "black", 
                                                  opacity = 1, 
                                                  bringToFront = T)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~overall_Merge$overall_FI_Rate,
            title = "Food Insecurity Rate from 2009 to 2018", 
            opacity = 0.7)
# Output map
map_interactive
saveWidget(map_interactive, "overallFIRateMap.html")

### Making a Shiny app - way to use R to create a web app
library(shiny)
### Publishing Shiny app
#install.packages('rsconnect')
rsconnect::setAccountInfo(name='savannahmilburn', token='A97FF9E978C1D8B7D50DFA320AABBB03', secret='sycIz2Uk9z94kP1enHJNO/b4K+zJkF6BgDw6WlNd')
library(rsconnect)