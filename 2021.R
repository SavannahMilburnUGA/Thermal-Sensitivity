# Analyzing data from 2021 - 74 sites from CRB + 1 AREMP site 

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# WANT: data frame of Logger - Series # - Date - Time - Temp in C 
# 	1. Creating site table using the Filename from master spreadsheet
		# a. Columns = lat, long, siteID
# Eventually other site attributes/landscape variables, and daily mean/max from 2021 (only commonly sampled dates)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Reading in 2021 CRB data
library(readxl)  
fileCRB2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/streamTemp/2021/ClackData2021.xlsx"
# Check for file exists
file.exists(fileCRB2021)
print(fileCRB2021)

# Get 2021 Clackamas Data Sheet Names
sites2021 <- excel_sheets(fileCRB2021)
# Print 2021 Clackamas Data Sheet Names: 76 sites + 4 USGS sites
sites2021
# Checking sites - should be 80
length(sites2021)
# Read all sheets in 2021 Clackamas Data to list of data frame objects (each site/sheet is a df object)
listOfSites <- lapply(sites2021, function(x) {as.data.frame(read_excel(file2021, sheet = x))})
# Rename elements in list to be the names of the sheet names
names(listOfSites) <- sites2021
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Checking listOfSites:
# Checking sheets read
length(listOfSites)
length(sites2021)
# Check names of list elements
names(listOfSites)
# Check structure of list: all have 5 variables
str(listOfSites, max.level=1)
# View first few rows of each sheet
lapply(listOfSites, head, n = 3)
# Look at a specific sheet
head(listOfSites[["2400469"]])
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Comparing to find overlapping dates across 2021 sites:
# Creating summary dataframe: 
# Site - Start Date - End date - Total Days Record - Missing Days
siteSummary2021 <- data.frame(siteID = character(), startDate = as.Date(character()), endDate = as.Date(character()), totalDays = integer(), missingDays = integer(), stringsAsFactors = FALSE)

# Iterate through list of sites to find start date, end date, total days, missing days for each site in 2021
# Note VS Code doesn't interpret R correctly when you highlight (needs to all be on one line so have to click Run button instead of highlighting + Enter)
{
    for(site in names(listOfSites)) {
    siteDF <- listOfSites[[site]]
    # Track start date, end date
    startDate <- as.Date(min(siteDF$Date, na.rm=TRUE))
    endDate <- as.Date(max(siteDF$Date, na.rm=TRUE))
    # Get unique dates to track total days recorded
    uniqueDates <- unique(as.Date(siteDF$Date))
    totalDays <- length(uniqueDates)
    # Find missing days
    missingDays <- (as.numeric(endDate - startDate) + 1) - totalDays
    # Adding to summary dataframe
    addSite <- data.frame(siteID=site, startDate=startDate, endDate=endDate, totalDays=totalDays, missingDays=missingDays)
    siteSummary2021 <- rbind(siteSummary2021, addSite)
    print(paste("Added site:", site, "- Total rows now:", nrow(siteSummary2021)))
}
}
## Checking summary table for 2021 
nrow(siteSummary2021)
siteSummary2021
# Saving siteSummary2021 as CSV file
write.csv(siteSummary2021, "siteSummary2021.csv", row.names=FALSE)
## Conclusions: (see below for specifics: where dates are missing)
# Site 10598088 has 47 missing days in 6/23/2020 - 10/26/2021
# Site 20733169 has 5 missing days in 6/23/2020 - 10/7/2021
# USGS Site 3 has 2 missing days in 6/15/2021 - 10/1/2021 

## Finding overlapping dates
# Compare via latest start date & earliest end date (later start date means all prior dates included, vice versa for end date)
library(ggplot2)
library(lubridate)
# Create a frequency table for the start dates
freqStartDates <- table(siteSummary2021$startDate)
barplot(freqStartDates, main = "Frequency of Start Dates for 2021 CRB Sites", xlab = "Start Date", ylab = "# Sites", las =2)
length(freqStartDates)
print(freqStartDates)
## FINDINGS: 
# 23 sites start at 12/31/2020 , 10 sites start at 6/23/2021
# If we start at 7/22/2021 all sites included ?
## SITES w/ MISSING DATES & where MISSING DATES OCCURRING
## Not site 10598088 since missing 8/6/2021 to 9/21/2021
## Not site 20733169 missing 8/20/2021 to 8/24/2021
# If we start at 7/1 - all sites except 5 included

# Create a frequency table for the end dates
freqEndDates <- table(siteSummary2021$endDate)
barplot(freqEndDates, main = "Frequency of End Dates for 2021 CRB Sites", xlab = "End Date", ylab = "# Sites", las =2)
length(freqEndDates)
print(freqEndDates)
# 10 sites end at 10/1/2021, 11 sites end at 10/22/2021, 11 sites end at 10/26/2021
# If we end at 8/2/2021 (8/1/2021 cleaner) all sites included ?
# If we end at 9/1/2021 - all sites except 1
# If we end at 10/1/2021 - all sites except 8
# ------------------------------------------------------------------------------------------------------------------------------------------------
# Need 74 CRB sites for July 1 - Aug 31 (7/1/2021 - 8/31/2021)
# Check site 10389421, 20012617 (ending 8/2)  ?
# 1 AREMP site

## Remove unneeded sites from CRB 2021 data & find daily stream temperature values: mean, min max
# Removing Site 10598088, Site 20733169, USGS Site 3 since have missing dates in 7/1/2021 - 8/31/2021
# Removing 10361309, 10931479, 11007911, 11007937, 20539817 bc. start date is later than 7/1/2021
sitesToRemove2021 <- c("10598088", "20733169", "Clackamas.Temperatures.USGS (3)", "10361309", "10931479", "11007911", "11007937", "20539817")
listOfSites2021 <- listOfSites[!names(listOfSites) %in% sitesToRemove2021]
length(listOfSites2021) # Check should be 72
## Find daily mean, min, max stream temperatures for 72 sites in 2021 CRB data
library(tidyverse)
# Iterature over each data frame object (site) in new listOfSites2021 list
CRBStreamTemperatureMMM2021 <- map_dfr(listOfSites2021, function(df) {
    df %>%
        rename(dateTime = 'Date', time = 'Time', temperature = 'Temp, °C') %>%
        mutate(date = as.Date(dateTime)) %>% # Extracting only date
        # Group rows in data frame by same data to find daily stream temperature stats
        group_by(date) %>%
        # Calculate stream temperature values and ignore any NAs/missing
        summarise(
            dailyMeanST = round(mean(temperature, na.rm = TRUE), 3),
            dailyMinST = round(min(temperature, na.rm = TRUE), 3),
            dailyMaxST = round(max(temperature, na.rm = TRUE), 3),
            # Also recording time of min & max - returns first occurrence if duplicates
            timeMinST = .data$time[which.min(temperature)],
            timeMaxST = .data$time[which.max(temperature)],
            .groups = 'drop' # Removing grouping of dates after
        )
}, .id = "siteID") # Adding column for site ID

# Add 1 AREMP site


# Need to combine AREMP + USGS sites + get coordinates
## Getting 2021 CRB data coordinates for 74 sites
# coordinatesFile2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/streamTemp/2021/Water temp data logs master 2021.xlsx"
# coordinates2021 <- read_excel(coordinatesFile2021) %>%
#     select(siteID = 'Filename of download', lat = 'GPS lat.', lon = 'GPS long.') %>%
#     distinct()