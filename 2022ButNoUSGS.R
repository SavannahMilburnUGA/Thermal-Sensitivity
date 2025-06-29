# Cleaning data for 2022 Clackamas Data - 2022 CRB data does NOT have 2022 USGS data

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off() 

# WANT: data frame of Logger - Datetime - Temp, C 
# Sheets also have graphs btw
# Note: -- sites/sheet
# 	1. Creating site table using the Filename from master spreadsheet
		# a. Columns = lat, long, siteID
# Eventually other site attributes/landscape variables, and daily mean/max from 2021 (only commonly sampled dates)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Reading in 2022 CRB data
library(readxl)  
file2022 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/streamTemp/2022/Copy of 2022_all_data_exceptAREMP&USGS.xlsx"
# Check for file exists
file.exists(file2022)
print(file2022)

# Get 2022 Clackamas Data Sheet Names
sites2022 <- excel_sheets(file2022)
# Print 2022 Clackamas Data Sheet Names: 77 sites
sites2022
# Checking sites - should be 77
length(sites2022)
# Read all sheets in 2022 Clackamas Data to list of data frame objects (each site/sheet is a df object)
listOfSites2022 <- lapply(sites2022, function(x) {as.data.frame(read_excel(file2022, sheet = x))})
# Rename elements in list to be the names of the sheet names
names(listOfSites2022) <- sites2022
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Checking listOfSites2022:
# Checking sheets read
length(listOfSites2022)
length(sites2022)
# Check names of list elements
names(listOfSites2022)
# Check structure of list: all have 5 variables but:
# Certain sites have MANY more observations
str(listOfSites2022, max.level=1)
# Check dimensions of each sheet
lapply(listOfSites2022, dim)
# Check column names for each sheet
lapply(listOfSites2022, names)
# View first few rows of each sheet
lapply(listOfSites2022, head, n = 3)
# Check data types in each sheet
lapply(listOfSites2022, function(x) sapply(x, class))
# Look at a specific sheet
head(listOfSites2022[["2400469"]])
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Comparing to find overlapping dates across 2022 sites:
# Creating summary dataframe: 
# Site - Start Date - End date - Total Days Record - Missing Days
siteSummary2022 <- data.frame(siteID = character(), startDate = as.Date(character()), endDate = as.Date(character()), totalDays = integer(), missingDays = integer(), stringsAsFactors = FALSE)

# Iterate through list of sites to find start date, end date, total days, missing days for each site in 2022
{
    for(site in names(listOfSites2022)) {
    siteDF <- listOfSites2022[[site]]
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
    siteSummary2022 <- rbind(siteSummary2022, addSite)
    print(paste("Added site:", site, "- Total rows now:", nrow(siteSummary2022)))
}
}
## Checking summary table for 2022
nrow(siteSummary2022)
siteSummary2022
# Saving siteSummary2022 as CSV file
write.csv(siteSummary2022, "siteSummary2022.csv", row.names=FALSE)
## Conclusions: (see below for specifics: where dates are missing)
# Site 20941696 has 22 missing days in 6/17/2022 - 10/14/2022
# Site 20606608 has 9 missing days in 6/8/2022 - 10/1/2022

## Finding overlapping dates
# Compare via latest start date & earliest end date (later start date means all prior dates included, vice versa for end date)
library(ggplot2)
library(lubridate)
# Create a frequency table for the start dates
freqStartDates <- table(siteSummary2022$startDate)
barplot(freqStartDates, main = "Frequency of Start Dates for 2022 CRB Sites", xlab = "Start Date", ylab = "# Sites", las =2)
length(freqStartDates)
print(freqStartDates)
## FINDINGS: 
# 22 sites start at 6/1/2022 , 8 sites start at 6/8/2022
# If we start at 8/13/2022 all sites included ?
## SITES w/ MISSING DATES & where MISSING DATES OCCURRING
## Not site 20941696 since missing 6/28/2022 to 7/19/2022
## Not site 20606608 since missing 6/21/2022 to 6/29/2022
# If we start at 6/30/2022 - all sites except 3 included

# Create a frequency table for the end dates
freqEndDates <- table(siteSummary2022$endDate)
barplot(freqEndDates, main = "Frequency of End Dates for 2022 CRB Sites", xlab = "End Date", ylab = "# Sites", las =2)
length(freqEndDates)
print(freqEndDates)
## FINDINGS: 
# 19 sites end at 10/1/2022 , 7 sites end at 10/4/2022, 
# 7 sites end at 10/7/2022, 10 sites end at 10/17/2022
# If we end at 10/1/2021 - all sites included ? - why is it 2021
# If we end at 8/24/2022 - all sites except 1 (could end at 8/1/2022)
# If we end at 9/1/2022 - all sites except 2
# If we end at 10/1/2022 - all sites except 9