#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## Clean house & remove saved files
rm(list = ls())
while (!is.null(dev.list())) dev.off()

library(shiny)
library(leaflet)
library(plotly)

# Loading 73 site coordinates to place clickable markers on rendered Leaflet map
coordinates2021 <- readRDS("coordinates2021.RDS")

# Checking coordinates2021
coordinates2021

# Define UI
ui <- fluidPage(
    titlePanel("Thermal Sensitivity in Clackamas River Basin"),
    
    sidebarLayout(
        sidebarPanel(
            h5("Click on a site to view air-stream temperature data and thermal sensitivity"),
            # Controls later ?
        ),
        
        mainPanel(
           tabsetPanel(
             tabPanel("Map of Sites", leafletOutput("siteMap")),
             tabPanel("Air-Stream Temperature Time-Series Graph", plotlyOutput("tempGraphs")),
             tabPanel("Thermal Sensitivity", plotlyOutput("thermalSensitivity"))
           )
        )
    )
)

# Define server
server <- function(input, output) {
    
    # Render the map with clickable sites
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
    
    # For now, just show which site was clicked
    output$tempGraphs <- renderPlotly({
        if(is.null(input$siteMap_marker_click)) {
            p <- plot_ly() %>%
                add_text(x = 0.5, y = 0.5, text = "Click a site on the map to view temperature data",
                        showlegend = FALSE) %>%
                layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                       yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""))
        } else {
            clicked_site <- input$siteMap_marker_click$id
            p <- plot_ly() %>%
                add_text(x = 0.5, y = 0.5, text = paste("You clicked:", clicked_site),
                        showlegend = FALSE) %>%
                layout(title = paste("Temperature Data for", clicked_site))
        }
        p
    })
    
    output$thermalSensitivity <- renderPlotly({
        if(is.null(input$siteMap_marker_click)) {
            p <- plot_ly() %>%
                add_text(x = 0.5, y = 0.5, text = "Click a site on the map to view thermal sensitivity",
                        showlegend = FALSE) %>%
                layout(xaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""),
                       yaxis = list(showgrid = FALSE, showticklabels = FALSE, title = ""))
        } else {
            clicked_site <- input$siteMap_marker_click$id
            p <- plot_ly() %>%
                add_text(x = 0.5, y = 0.5, text = paste("Thermal sensitivity for:", clicked_site),
                        showlegend = FALSE) %>%
                layout(title = paste("Thermal Sensitivity for", clicked_site))
        }
        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)