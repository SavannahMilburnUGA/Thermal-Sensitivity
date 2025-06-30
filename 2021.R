# Getting stream temperature & air temperature values for 72 from CRB + 1 AREMP site 
# Details for 2021 CRB Data found in 2021 branch
# Details for 2021 AREMP Data found in 2021AREMP branch 

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# Data frame formats are - for combining after PRISM values gotten
# CRB: Logger - Series # - Date - Time - Temp, C 
# AREMP: deploy - date - Year - time - tempC


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

# Finding unique sites for CRB 2021 Data
uniqueSiteIDs <- unique(CRBStreamTemperatureMMM2021$siteID)
uniqueSiteIDs
length(uniqueSiteIDs) # 72 unique sites
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
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Match coordinates to CRB 2021 sites
coordinatesFile2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/streamTemp/2021/Water temp data logs master 2021.xlsx"
# Check if file exists
file.exists(coordinatesFile2021)
# Skip header info in coordinate file
coordinates2021 <- read_excel(coordinatesFile2021, skip = 5)
# Select only coordinates and site ID - E, F, N columns
coordinates2021 <- coordinates2021 %>%
    select(`GPS lat.`, `GPS long.`, `Filename of downloaded data`)
# Convert siteID out of scientific notation
coordinates2021 <- coordinates2021 %>%
    filter(!is.na(`Filename of downloaded data`) & 
           `Filename of downloaded data` != "" &
           !is.na(as.numeric(`Filename of downloaded data`))) %>%  # Keep only valid numeric entries
    mutate(siteID = sprintf("%.0f", as.numeric(`Filename of downloaded data`))) %>%
    select(lat = `GPS lat.`, lon = `GPS long.`, siteID) %>%
    distinct()
# Only keep coordinates for 72 unique sites I am including in analysis for 2021 Test Case
coordinates2021 <- coordinates2021 %>%
  filter(siteID %in% uniqueSiteIDs)
length(uniqueSiteIDs)
nrow(coordinates2021)

# 72 unique sites for CRB 2021 data, 68 unique sites w/ coordinates = 4 unique sites that need coordinates added
# Adding 4 unique sites missing their coordinates: Site 20201001A & 3 USGS sites: 1, 2, 4
coordinates2021 <- coordinates2021 %>%
    add_row(siteID = "20201001A", lat = 45.412205, lon = -122.505561) %>%
    add_row(siteID = "USGS 1", lat = 45.16722, lon = -122.155) %>%
    add_row(siteID = "USGS 2", lat = 45.3, lon = -122.35278) %>%
    add_row(siteID = "USGS 4", lat = 45.14778, lon = -122.15194)
nrow(coordinates2021) # 72
# ------------------------------------------------------------------------------------------------------------------------------------------------
# Adding 1 AREMP site to coordinates2021
coordinates2021 <- coordinates2021 %>%
    add_row(siteID = "ORCUB001IN02", lat = 44.886616596029, lon = -121.883197021033) 
# Checking
nrow(coordinates2021) # 73
coordinates2021
# Save coordinates2021 file 
write.csv(coordinates2021, "coordinates2021.csv", row.names=FALSE)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Extracting 2021 July 1 - Aug 31, 2021 Air Temperature data (daily mean, min, max)
# Load necessary packages
library(prism)   
library(terra)  
library(dplyr)
library(tidyr)  
# Set the folder where PRISM raster files will be saved
prism_set_dl_dir("C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/data/airTemp/2021")
# Define the years and the summer date range: July 1, 2021 - Aug 31, 2021
yr <- 2021
start_day <- "-07-01"
end_day   <- "-08-31"
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Download daily mean, min, max air temperature rasters from PRISM
# Download daily mean air temperature rasters from PRISM
get_prism_dailys(type = "tmean", minDate = paste0(yr, start_day), maxDate = paste0(yr, end_day), keepZip = FALSE)
# Download daily min air temperature rasters from PRISM
get_prism_dailys(type = "tmin", minDate = paste0(yr, start_day), maxDate = paste0(yr, end_day), keepZip = FALSE)
# Download daily max air temperature rasters from PRISM
get_prism_dailys(type = "tmax", minDate = paste0(yr, start_day), maxDate = paste0(yr, end_day), keepZip = FALSE)
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Prepare site table using coordinates 2021
site_table <- coordinates2021 %>%
    rename(site = siteID) # Rename to match PRISM
# Convert site table to a spatial object (SpatVector) with WGS84 coordinates (to match PRISM)
site_points <- vect(site_table, geom = c("lon", "lat"), crs = "EPSG:4326")
# Checking
head(site_table) # Coordinate data
nrow(site_table) # 73 unique sites
class(site_points) # SpatVector
crs(site_points) # WGS84 coordinates to match PRISM
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Extract and process daily mean air temperature data from PRISM
tmean_files <- prism_archive_subset("tmean", "daily", years = 2021, mon = 7:8)
tmean_filepaths <- pd_to_file(tmean_files)
# Checking daily mean air temperature
length(tmean_files) # 62 days
pd_get_name(tmean_files) # July 1, 2021 - Aug 31, 2021 Mean temperature

# Load the tmean rasters into a single raster stack (one layer per day)
tmean_stack <- rast(tmean_filepaths)

# Extract the daily temperature values for each site location from the raster stack
# Each row corresponds to a site; each column (after the first) is a daily value
tmean_vals <- terra::extract(tmean_stack, site_points)

# Checking extraction of daily mean temperature values for each site
dim(tmean_vals) # 73 x 63 = sites x ID 62 days
head(tmean_vals[, 1:5]) # First few columns

# Combine the extracted temperature values with the site names from the original table
# Drop the ID column returned by extract() and add site names
tmean_result <- cbind(site = site_table$site, tmean_vals[, -1])

# Rename the temperature columns using just the date portion from each raster layer name
# This makes the column names easier to read (e.g., "20230601" instead of full PRISM name)
tmean_dates <- substr(names(tmean_stack), start = 26, stop = 33)
colnames(tmean_result)[-1] <- tmean_dates
# Checking date extraction was correct
substr(names(tmean_stack)[1:3], start=26, stop =33)
# Reshape to long format for analysis or plotting, also format dates
# One row per site-date
tmean_long_result <- pivot_longer(tmean_result, -site, names_to = "date", values_to = "tmean_C")
# Convert date strings to Date objects
tmean_long_result$date <- as.Date(tmean_long_result$date, format = "%Y%m%d") 
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Extract and process daily min air temperature data from PRISM
tmin_files <- prism_archive_subset("tmin", "daily", years = 2021, mon = 7:8)
tmin_filepaths <- pd_to_file(tmin_files)
# Checking daily min air temperature
length(tmin_files) # 62 days
pd_get_name(tmin_files) # July 1, 2021 - Aug 31, 2021 Minimum temperature

# Load the tmin rasters into a single raster stack (one layer per day)
tmin_stack <- rast(tmin_filepaths)

# Extract the daily temperature values for each site location from the raster stack
# Each row corresponds to a site; each column (after the first) is a daily value
tmin_vals <- terra::extract(tmin_stack, site_points)

# Checking extraction of daily min temperature values for each site
dim(tmin_vals) # 73 x 63 = sites x ID 62 days
head(tmin_vals[, 1:5]) # First few columns

# Combine the extracted temperature values with the site names from the original table
# Drop the ID column returned by extract() and add site names
tmin_result <- cbind(site = site_table$site, tmin_vals[, -1])

# Rename the temperature columns using just the date portion from each raster layer name
# This makes the column names easier to read (e.g., "20230601" instead of full PRISM name)
tmin_dates <- substr(names(tmin_stack), start = 25, stop = 32)
colnames(tmin_result)[-1] <- tmin_dates
# Checking date extraction was correct
substr(names(tmin_stack)[1:3], start=25, stop =32)
# Reshape to long format for analysis or plotting, also format dates
# One row per site-date
tmin_long_result <- pivot_longer(tmin_result, -site, names_to = "date", values_to = "tmin_C")
# Convert date strings to Date objects
tmin_long_result$date <- as.Date(tmin_long_result$date, format = "%Y%m%d") 
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Extract and process daily max air temperature data from PRISM
tmax_files <- prism_archive_subset("tmax", "daily", years = 2021, mon = 7:8)
tmax_filepaths <- pd_to_file(tmax_files)
# Checking daily max air temperature
length(tmax_files) # 62 days
pd_get_name(tmax_files) # July 1, 2021 - Aug 31, 2021 Maximum temperature

# Load the tmean rasters into a single raster stack (one layer per day)
tmax_stack <- rast(tmax_filepaths)

# Extract the daily temperature values for each site location from the raster stack
# Each row corresponds to a site; each column (after the first) is a daily value
tmax_vals <- terra::extract(tmax_stack, site_points)

# Checking extraction of daily max temperature values for each site
dim(tmax_vals) # 73 x 63 = sites x ID 62 days
head(tmax_vals[, 1:5]) # First few columns

# Combine the extracted temperature values with the site names from the original table
# Drop the ID column returned by extract() and add site names
tmax_result <- cbind(site = site_table$site, tmax_vals[, -1])

# Rename the temperature columns using just the date portion from each raster layer name
# This makes the column names easier to read (e.g., "20230601" instead of full PRISM name)
tmax_dates <- substr(names(tmax_stack), start = 25, stop = 32)
colnames(tmax_result)[-1] <- tmax_dates
# Checking date extraction was correct
substr(names(tmax_stack)[1:3], start=25, stop =32)
# Reshape to long format for analysis or plotting, also format dates
# One row per site-date
tmax_long_result <- pivot_longer(tmax_result, -site, names_to = "date", values_to = "tmax_C")
# Convert date strings to Date objects
tmax_long_result$date <- as.Date(tmax_long_result$date, format = "%Y%m%d") 
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Combine daily tmean, tmin, tmax air temperature datasets from PRISM
airTemperature2021 <- tmean_long_result %>%
    left_join(tmin_long_result, by = c("site", "date")) %>%
    left_join(tmax_long_result, by = c("site", "date"))
# Checking airTemperature2021 
head(airTemperature2021)
nrow(airTemperature2021) # Should be 4,526 rows (73 sites × 62 days)
ncol(airTemperature2021) # Should be 5 columns (site, date, tmean_C, tmin_C, tmax_C)
range(airTemperature2021$date) # 7/1/2021 - 8/31/2021
# ------------------------------------------------------------------------------------------------------------------------------------------------
## Saving PRISM results
# Saving combined daily tmean, tmin, tmax air temperature data
saveRDS(airTemperature2021, "airTemperature2021.rds")
write_csv(airTemperature2021, "airTemperature2021.csv")
# Saving tmean, tmin, tmax air temperature datasets individually
saveRDS(tmean_long_result, "tmean_long_result.rds")
write_csv(tmean_long_result, "tmean_long_result.csv")
saveRDS(tmin_long_result, "tmin_long_result.rds")
write_csv(tmin_long_result, "tmin_long_result.csv")
saveRDS(tmax_long_result, "tmax_long_result.rds")
write_csv(tmax_long_result, "tmax_long_result.csv")
