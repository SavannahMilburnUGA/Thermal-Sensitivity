## Calculating thermal sensitivities for each of the 73 sites
library(broom)
library(tidyverse)
# Loading RDS structures - using R in VS Code is finnicky
AREMPStreamTemperatureMMM2021 <- readRDS("AREMPDailyStreamTemps2021.rds")
CRBStreamTemperatureMMM2021 <- readRDS("CRBDailyStreamTemps2021.rds")
airTemperature2021 <- readRDS("airTemperature2021.rds")
coordinates2021 <- readRDS("coordinates2021.rds")
# Create empty data frame
thermalSensitivities2021 <- data.frame()
# Get all 73 site IDs
allSites <- coordinates2021$siteID

# Iterate through 73 sites to get daily mean stream and air temperatures - calculate thermal sensitivities 
{
for (siteN in allSites) {
    # Determine if site is the 1 AREMP site or 1 of 72 CRB sites
    if (siteN %in% AREMPStreamTemperatureMMM2021$siteID) {
        siteStreamData <- AREMPStreamTemperatureMMM2021 %>% filter(siteID == siteN) %>%
        select(date, dailyMeanST)
    } else {
        siteStreamData <- CRBStreamTemperatureMMM2021 %>% filter(siteID == siteN) %>%
        select(date, dailyMeanST)
    }
    # Get daily mean air temperature for this stie
    siteAirData <- airTemperature2021 %>%
        filter(site == siteN) %>%
        select(date, tmean_C)
    # Combine daily mean stream and air temperature data for site
    siteMeanData <- siteStreamData %>%
        inner_join(siteAirData, by = "date") %>%
        filter(!is.na(dailyMeanST) & !is.na(tmean_C))
    # Regress mean air temperature & mean stream temperature
    linModel <- lm(dailyMeanST ~ tmean_C, data = siteMeanData)
    # Find summary statistics
    modelGlance <- glance(linModel)
    modelTidy <- tidy(linModel)
    thermalSensitivity <- modelTidy$estimate[2]
    intercept <- modelTidy$estimate[1]
    rSquared <- modelGlance$r.squared
    adjRSquared <- modelGlance$adj.r.squared
    rmse <- sqrt(modelGlance$deviance / modelGlance$df.residual)
    meanStreamTemp <- mean(siteMeanData$dailyMeanST, na.rm = TRUE)
    meanAirTemp <- mean(siteMeanData$tmean_C, na.rm = TRUE)
    sdStreamTemp <- sd(siteMeanData$dailyMeanST, na.rm = TRUE)
    sdAirTemp <- sd(siteMeanData$tmean_C, na.rm = TRUE)
    minStreamTemp <- min(siteMeanData$dailyMeanST, na.rm = TRUE)
    maxStreamTemp <- max(siteMeanData$dailyMeanST, na.rm = TRUE)
    minAirTemp <- min(siteMeanData$tmean_C, na.rm = TRUE)
    maxAirTemp <- max(siteMeanData$tmean_C, na.rm = TRUE)
    rangeStreamTemp <- maxStreamTemp - minStreamTemp
    rangeAirTemp <- maxAirTemp - minAirTemp
    # Get coordinates for each site
    siteCoords <- coordinates2021 %>% filter(siteID == siteN)
    # Create row to add to overall data frame for site
    # Adding x, y coordinates to mesh EVs: lat: N-S = y & lon = E-W = x
    siteRow <- data.frame(site = siteN, x = siteCoords$lon, y = siteCoords$lat, thermalSensitivity = round(thermalSensitivity, 3), intercept = round(intercept, 3), rSquared = round(rSquared, 3), adjRSquared = round(adjRSquared, 3), rmse = round(rmse, 3), meanStreamTemp = round(meanStreamTemp, 3), meanAirTemp = round(meanAirTemp, 3), sdStreamTemp = round(sdStreamTemp, 3), sdAirTemp = round(sdAirTemp, 3), minStreamTemp = round(minStreamTemp, 3), maxStreamTemp = round(maxStreamTemp, 3), minAirTemp = round(minAirTemp, 3), maxAirTemp = round(maxAirTemp, 3), rangeStreamTemp = round(rangeStreamTemp, 3), rangeAirTemp = round(rangeAirTemp, 3))
    # Add row to overall thermal sensitivity data frame
    thermalSensitivities2021 <- rbind(thermalSensitivities2021, siteRow)
    }
}
# Checking
nrow(thermalSensitivities2021) #73
View(thermalSensitivities2021)

# Saving thermal sensitivites for 2021 73 sites
saveRDS(thermalSensitivities2021, "thermalSensitivities2021.RDS")
write_csv(thermalSensitivities2021, "thermalSensitivities2021.csv")