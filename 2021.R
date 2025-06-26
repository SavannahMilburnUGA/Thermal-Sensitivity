# Cleaning data for 2021 Clackamas Data 

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# WANT: data frame of Logger - Series # - Date - Time - Temp in C 
# Sheets also have graphs btw
# Note: 76 sites/sheet & 4 USGS sites/sheets
# 	1. Creating site table using the Filename from master spreadsheet
		# a. Columns = lat, long, siteID
# Eventually other site attributes/landscape variables, and daily mean/max from 2021 (only commonly sampled dates)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Reading in 2021 CRB data
library(readxl)  
file2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/ClackData2021.xlsx"
# Check for file exists
file.exists(file2021)
print(file2021)

# Get 2021 Clackamas Data Sheet Names
sites2021 <- excel_sheets(file2021)
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
# Check structure of list: all have 5 variables but:
# Certain sites have MANY more observations
str(listOfSites, max.level=1)
# Check dimensions of each sheet
lapply(listOfSites, dim)
# Check column names for each sheet
lapply(listOfSites, names)
# View first few rows of each sheet
lapply(listOfSites, head, n = 3)
# Check data types in each sheet
lapply(listOfSites, function(x) sapply(x, class))
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