# Getting stream temperature & air temperature values for 72 from CRB + 1 AREMP site  

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# WANT: data frame of Logger - Series # - Date - Time - Temp in C 
## Create data frame of deploy - date - Year - time - tempC


### Getting 2021 Stream Temperature Data: 72 sites from CRB + 1 AREMP site: 
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Reading in 2021 CRB data: 
library(readxl)  
fileCRB2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/streamTemp/2021/ClackData2021.xlsx"
# Check for file exists
file.exists(fileCRB2021)
print(fileCRB2021)

# Get 2021 Clackamas Data Sheet Names: site IDs
sites2021 <- excel_sheets(fileCRB2021)
# Print 2021 Clackamas Data Sheet Names: 76 sites + 4 USGS sites
sites2021
# Checking sites
length(sites2021) # 80
# Read all sheets in 2021 Clackamas Data to list of data frame objects (each site/sheet is a df object)
# Create data frame of Logger - Series # - Date - Time - Temp, C 
listOfSites <- lapply(sites2021, function(x) {as.data.frame(read_excel(fileCRB2021, sheet = x))})
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
# Need 74 CRB sites for July 1 - Aug 31 (7/1/2021 - 8/31/2021)
# Check site 10389421, 20012617 (ending 8/2)  ?
# 1 AREMP site

## Remove unneeded sites from CRB 2021 data & find daily stream temperature values: mean, min, max
# Removing Site 10598088, Site 20733169, USGS Site 3 since have missing dates in 7/1/2021 - 8/31/2021
# Removing 10361309, 10931479, 11007911, 11007937, 20539817 because start date is later than 7/1/2021
sitesToRemove2021 <- c("10598088", "20733169", "Clackamas.Temperatures.USGS (3)", "10361309", "10931479", "11007911", "11007937", "20539817")
# New list of CRB 2021 cites (includes 69/76 CRB sites + 3/4 USGS sites)
listOfSites2021 <- listOfSites[!names(listOfSites) %in% sitesToRemove2021]
length(listOfSites2021) # Check should be 72
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Find daily mean, min, max stream temperatures for 72 sites in 2021 CRB data
library(tidyverse)
# Iterate over each data frame object (site) in new listOfSites2021 list
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

# Output
CRBStreamTemperatureMMM2021
nrow(CRBStreamTemperatureMMM2021) # 12953
# Save CRB 2021 stream temperature data frame
saveRDS(CRBStreamTemperatureMMM2021, "CRBDailyStreamTemps2021.rds")
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Add 1 AREMP site: deploy_ORCUB001IN02 : 1/1/2021 - 12/31/2021 // has FULL YEAR
# Reading in 2021 AREMP data
library(readxl)  
fileAREMP2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/streamTemp/2021/AREMP2_2021_IN.csv"
# Check for file exists
file.exists(fileAREMP2021)
print(fileAREMP2021)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Create data frame of deploy - date - Year - time - tempC
sitesAREMP2021 <- read.csv(fileAREMP2021)
str(sitesAREMP2021)
# Types are chr, chr, int, chr, num
# Convert chr date to Date
sitesAREMP2021$date <- as.Date(sitesAREMP2021$date, format = "%m/%d/%Y")
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Get unique deploy IDs/AREMP sites
uniqueDeploys <- unique(sitesAREMP2021$deploy)
length(uniqueDeploys)
print(uniqueDeploys)
# Only 1 deploy has compatible dates
deploy_ORCUB001IN02 <- sitesAREMP2021[sitesAREMP2021$deploy == "ORCUB001IN02", ]
head(deploy_ORCUB001IN02)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Find daily mean, min, max stream temperatures for 1 2021 AREMP site: 
AREMPStreamTemperatureMMM2021 <- deploy_ORCUB001IN02 %>%
    mutate(dateAREMP = as.Date(date)) %>%
    # Group rows in data frame by same data to find daily stream temperature stats
    group_by(dateAREMP) %>%
    # Calculate stream temperature values and ignore any NAs/missing
    summarise(
        dailyMeanST = round(mean(tempC, na.rm = TRUE), 3),
        dailyMinST = round(min(tempC, na.rm = TRUE), 3),
        dailyMaxST = round(max(tempC, na.rm = TRUE), 3),
        # Also recording time of min & max - returns first occurrence if duplicates
        timeMinST = .data$time[which.min(tempC)],
        timeMaxST = .data$time[which.max(tempC)],
        .groups = 'drop' # Removing grouping of dates after
    ) %>%
    rename(date = dateAREMP) %>%
    mutate(siteID = "ORCUB001IN02")

# Output
AREMPStreamTemperatureMMM2021
nrow(AREMPStreamTemperatureMMM2021) # 365
# Save 1 AREMP 2021 stream temperature data frame
saveRDS(AREMPStreamTemperatureMMM2021, "AREMPDailyStreamTemps2021.rds")



# 	1. Creating site table using the Filename from master spreadsheet
		# a. Columns = lat, long, siteID
# Eventually other site attributes/landscape variables, and daily mean/max from 2021 (only commonly sampled dates)## Getting 2021 CRB data coordinates for 74 sites
# coordinatesFile2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/streamTemp/2021/Water temp data logs master 2021.xlsx"
# coordinates2021 <- read_excel(coordinatesFile2021) %>%
#     select(siteID = 'Filename of download', lat = 'GPS lat.', lon = 'GPS long.') %>%
#     distinct()