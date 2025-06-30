# Cleaning data for 2021 AREMP Data 

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# WANT: data frame of deploy - date - Year - time - tempC
# Note: 4 deploys only
# 	1. Creating site table using the Filename from master spreadsheet
		# a. Columns = lat, long, siteID
# Eventually other site attributes/landscape variables, and daily mean/max from 2021 (only commonly sampled dates)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Reading in 2021 AREMP data
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
# 4 deploys: ORCUB001IN02, ORHOT001IN01, ORHRK001IN01, ORHRK001IN02
# Separate AREMP 2021 data into 4 data frame objects by 4 deploy IDs
deploy_ORCUB001IN02 <- sitesAREMP2021[sitesAREMP2021$deploy == "ORCUB001IN02", ]
head(deploy_ORCUB001IN02)
deploy_ORHOT001IN01 <- sitesAREMP2021[sitesAREMP2021$deploy == "ORHOT001IN01", ]
deploy_ORHRK001IN01 <- sitesAREMP2021[sitesAREMP2021$deploy == "ORHRK001IN01", ]
deploy_ORHRK001IN02 <- sitesAREMP2021[sitesAREMP2021$deploy == "ORHRK001IN02", ]
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Checking each deploy dataframe object right
nrow(sitesAREMP2021) # 17290
nrow(deploy_ORCUB001IN02)+nrow(deploy_ORHOT001IN01)+nrow(deploy_ORHRK001IN01)+nrow(deploy_ORHRK001IN02) # 17290
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Finding date ranges
# Creating summary data frame for AREMP 2021 data
AREMP2021Summary <- data.frame(deployID = character(), startDate = as.Date(character()), endDate = as.Date(character()), totalDays = integer(), missingDays = integer(), stringsAsFactors = FALSE)
# Creating a function to track start date, end date, total days recorded, missing days
analyzeDeploy <- function(deployDF, deployID) {
    # Track start date, end date
    startDate <- as.Date(min(deployDF$date, na.rm=TRUE))
    endDate <- as.Date(max(deployDF$date, na.rm=TRUE))
    # Get unique dates to track total days recorded
    uniqueDates <- unique(as.Date(deployDF$date))
    totalDays <- length(uniqueDates)
    # Find missing days
    missingDays <- (as.numeric(endDate - startDate) + 1) - totalDays
    # Return summary row
    return(data.frame(deploy_ID=deployID, startDate=startDate, endDate=endDate, totalDays=totalDays, missingDays=missingDays))
}
# Apply analyzeDeploy function to 4 deploy sites to find overlapping dates
AREMP2021Summary <- rbind(AREMP2021Summary, analyzeDeploy(deploy_ORCUB001IN02, "ORCUB001IN02"))
AREMP2021Summary <- rbind(AREMP2021Summary, analyzeDeploy(deploy_ORHOT001IN01, "ORHOT001IN01"))
AREMP2021Summary <- rbind(AREMP2021Summary, analyzeDeploy(deploy_ORHRK001IN01, "ORHRK001IN01"))
AREMP2021Summary <- rbind(AREMP2021Summary, analyzeDeploy(deploy_ORHRK001IN02, "ORHRK001IN02"))
AREMP2021Summary
# Saving AREMP2021Summary as CSV file
write.csv(AREMP2021Summary, "AREMP2021Summary.csv", row.names=FALSE)
## Conclusions: doesn't seem very able to mesh w/ CRB + 4 USGS sites from 2021...
# Only 1/4 deploys could be added to CRB + 4 USGS sites
# For deploy sites ONLY could do: 9/17/2021 to 5/25/2021 OR 9/17/2021 to 12/31/2021 EXCEPT 1: deploy_ORHOT001IN01
# deploy_ORCUB001IN02 : 1/1/2021 - 12/31/2021 // has FULL YEAR
# deploy_ORHOT001IN01 : 1/1/2021 - 5/25/2021 // won't make it thru
# deploy_ORHRK001IN01 : 9/17/2021 - 12/31/2021 // won't make it thru
# deploy_ORHRK001IN02 : 9/17/2021 - 12/31/2021 // won't make it thru