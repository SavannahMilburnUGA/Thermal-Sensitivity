#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# Loading libraries
library(shiny)
library(leaflet)
library(plotly)
library(tidyverse)
library(htmlwidgets)

# Sharing app ?
# Modifying to share app - working as of now 
#install.packages("conflicted")
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("layout", "plotly")

# Loading 73 site coordinates to place clickable markers on rendered Leaflet map
coordinates2021 <- readRDS("coordinates2021.RDS")
# Checking coordinates2021
coordinates2021

# Loading stream temperature datasets for 73 sites
streamDataCRB2021 <- readRDS("CRBDailyStreamTemps2021.rds")
head(streamDataCRB2021 ) # 72 sites
streamDataAREMP2021 <- readRDS("AREMPDailyStreamTemps2021.rds")
streamDataAREMP2021 # 1 site 
# Loading air temperature data for 73 sites from PRISM
airTempData2021 <- readRDS("airTemperature2021.RDS")
head(airTempData2021)

# Define UI for application that allows user to click on one of 73 sites to view
# air-stream temperature time-series & thermal sensitivity over 7/1/2021 - 8/31/2021
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", 
                 href = "https://fonts.googleapis.com/css2?family=Archivo:wght@400;700&display=swap")
    ),
    # Application title
    titlePanel("Air-Stream Temperature Relationship & Thermal Sensitivity in Clackamas River Basin from July 1, 2021 to August 31, 2021"),
    # Sidebar w/ instruction panel & tabs
    sidebarLayout(
        sidebarPanel(
            # Add link to raw data?
            h5("Please click on a site to view air-stream temperature time-series and thermal sensitivity from 7/1/2021 - 8/31/2021"),
        ),
        # Tabs
        mainPanel(
           tabsetPanel(
             tabPanel("Map of Sites", leafletOutput("siteMap")),
             tabPanel("Air-Stream Temperature Time-Series", plotlyOutput("tempGraphs")),
             tabPanel("Thermal Sensitivity", plotlyOutput("thermalSensitivity"))
           )
        )
    )
)

# Define server logic required to draw time-series
server <- function(input, output) {
    
    # Render the map with clickable sites w/ tabs
    output$siteMap <- renderLeaflet({
        coordinates2021 %>%
            leaflet() %>%
            setView(lat = 45.12, lng = -122.15, zoom = 9) %>%  # Zoom to CRB location
            # Setting map style rendered
            addProviderTiles(provider = "USGS.USTopo") %>%
            # Setting markers rendered for sites
            addCircleMarkers(lat = ~lat, lng = ~lon, 
                           layerId = ~siteID,
                           label = ~paste("Site:", siteID),
                           radius = 5,
                           fillOpacity = 1,
                           color = "blue")
    })

    # Creating a reactive function to filter data based on site choice
    # Returns merged air-stream temperature data for clicked site based on if site is AREMP/CRB site
    siteChoiceData <- reactive({
        req(input$siteMap_marker_click)
        clickedSite <- input$siteMap_marker_click$id
        # Determine if site choice is 1/72 CRB sites OR the 1 AREMP site
        if (clickedSite %in% streamDataAREMP2021$siteID) {
            # Clicked site is 1 AREMP site
            siteStreamData <- streamDataAREMP2021 %>% filter(siteID == clickedSite)
        } else if (clickedSite %in% streamDataCRB2021$siteID) {
            # Clicked site is 1 of 72 CRB sites
            siteStreamData <- streamDataCRB2021 %>% filter(siteID == clickedSite)
        } else {
            # Empty if clicked site not found in stream temp datasets
            return(data.frame())
        } 
        # Get air temperature data for clicked site
        siteAirData <- airTempData2021 %>% 
            filter(site == clickedSite)
        # Merge stream temperature data and air temperature data together based on site source (CRB or AREMP)
        siteData <- siteStreamData %>%
            left_join(siteAirData, by = "date") %>%
            filter(!is.na(tmean_C) & !is.na(dailyMeanST))  # Remove rows w/ NAs/missing data
        return(siteData)
    })
    
    # Building tab that produces air-stream temperature time-series
    # Includes range slider too
    output$tempGraphs <- renderPlotly({
        # Check if site was clicked on map
        if(is.null(input$siteMap_marker_click)) {
            graphAS <- plot_ly() %>%
                add_text(x = 0.5, y = 0.5, text = "Please click a site on the map to view air-stream temperature time-series.",
                        showlegend = FALSE) %>%
                layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), plot_bgcolor = "#e5ecf6", paper_bgcolor = "#e5ecf6")
        } else {
            # Site was clicked on map:
            # Assign site data from reactive function filtering to clicked site
            clickedSite <- input$siteMap_marker_click$id
            # Calling reactive function
            clickedSiteData <- siteChoiceData()
            
            # No data available for clicked site
            if(nrow(clickedSiteData) == 0) {
                graphAS <- plot_ly() %>%
                    add_text(x = 0.5, y = 0.5, text = paste("No air-stream temperature data available for site.", clickedSite),
                            showlegend = FALSE)
            # Plot air-temperature time series 
            } else {
                graphAS <- plot_ly(clickedSiteData, x = ~date) %>%
                    add_lines(y = ~tmean_C, name = "Mean Air Temperature", line = list(color = "#ab63f9", width = 2)) %>%
                    add_lines(y = ~dailyMeanST, name = "Mean Stream Temperature", line = list(color = "0bce9a", width = 2)) %>%
                    layout(
                        title = paste("Air-Stream Temperature Time Series for Site", clickedSite),
                        xaxis = list(
                            title = "Date",
                            rangeslider = list(visible = TRUE),
                            rangeselector = list(
                                buttons = list(
                                    list(count = 7, label = "1W", step = "day", stepmode = "backward"),
                                    list(count = 14, label = "2W", step = "day", stepmode = "backward"),
                                    list(count = 1, label = "1M", step = "month", stepmode = "backward"),
                                    list(step = "all", label = "All")
                                )
                            )
                        ),
                        yaxis = list(title = "Daily Mean Temperature (°C)"),
                        hovermode = "closest",
                        legend = list(x = 1.1, y = 0.5, font = list(color = "black", size = 12, family = "Archivo, Arial, san-serif")),
                        plot_bgcolor = "#e5ecf6",
                        paper_bgcolor = "#e5ecf6",
                        font = list(family = "Archivo, Arial, sans-serif", color = "black", size = 12), 
                        hoverdistance = 20, 
                        spikedistance = -1
                    )
            }
        }
        # Output time-series for air-stream temperature 
        graphAS
    })
    
    # Building tab that produces thermal sensitivity time-series
    output$thermalSensitivity <- renderPlotly({
        # Check if site was clicked on map
        if(is.null(input$siteMap_marker_click)) {
            graphTS <- plot_ly() %>%
                add_text(x = 0.5, y = 0.5, text = "Please click a site on the map to view air-stream temperature time-series.",
                        showlegend = FALSE) %>%
                layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), plot_bgcolor = "#e5ecf6", paper_bgcolor = "#e5ecf6")
        } else {
            # Site was clicked on map:
            # Assign site data from reactive function filtering to clicked site
            clickedSite <- input$siteMap_marker_click$id
            # Calling reactive function
            clickedSiteData <- siteChoiceData()
            
            # No data available for clicked site
            if(nrow(clickedSiteData) == 0) {
                graphTS <- plot_ly() %>%
                    add_text(x = 0.5, y = 0.5, text = paste("No air-stream temperature data available for site.", clickedSite),
                            showlegend = FALSE)
            # Plot thermal sensitivity time series 
            } else {
                # Calculate linear regression
                linReg <- lm(dailyMeanST ~ tmean_C, data = clickedSiteData)
                slopeTS <- round(coef(linReg)[2], 3)

                graphTS <- plot_ly(clickedSiteData, x = ~tmean_C, y = ~dailyMeanST) %>%
                    add_markers(color = I("#636efa"), size = I(8), name = "Daily Values") %>%
                        add_lines(x = ~tmean_C, y = ~fitted(linReg), 
                         line = list(color = "#fea15b", width = 2), 
                         name = paste("Thermal Sensitivity =", slopeTS)) %>%
                    layout(
                        title = paste("Thermal Sensitivity for Site", clickedSite),
                        xaxis = list(title = "Mean Air Temperature (°C)"),
                        yaxis = list(title = "Mean Stream Temperature (°C)"),
                        hovermode = "closest",
                        legend = list(x = 1.1, y = 0.5, font = list(color = "black", size = 12, family = "Archivo, Arial, san-serif")),
                        plot_bgcolor = "#e5ecf6",
                        paper_bgcolor = "#e5ecf6",
                        font = list(family = "Archivo, Arial, sans-serif", color = "black", size = 12), 
                        hoverdistance = 20, 
                        spikedistance = -1
                    )
            }
        }
        # Output regression for thermal sensitivity  
        graphTS
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



# ### Making a Shiny app - way to use R to create a web app
# library(shiny)
# ### Publishing Shiny app
# #install.packages('rsconnect')
# rsconnect::setAccountInfo(name='savannahmilburn', token='A97FF9E978C1D8B7D50DFA320AABBB03', secret='sycIz2Uk9z94kP1enHJNO/b4K+zJkF6BgDw6WlNd')
# library(rsconnect)