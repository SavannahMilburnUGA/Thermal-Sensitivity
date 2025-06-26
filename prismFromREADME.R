# Working thru example in README.md for PRISM 
# https://github.com/ropensci/prism#downloading-data

## PRISM Installation
library(devtools) # Needed to download PRISM from GitHub
# install.packages("reshape2") # Have to install reshape2 package
library(reshape2) # Melting dataframes
library(dplyr) # Data wrangling
library(raster) # Working w/ raster data
library(sp) # Manipulating spatial data
install_github("ropensci/prism")
# install.packages("prism") # Have to install prism package
library(prism) # PRISM data access
# Going to need tmean, tmax, tmin, ppt ? from PRISM

## Set directory that PRISM data will be saved to before downloading any data
prism_set_dl_dir("C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/PRISM")
# Now referred to as the "PRISM archive" - use prism_archive_*() to search through archive
# PRISM data referred to by folder names but "real" data in .bil/.txt/etc files 
# PRISM data accessed using pd_*()

## Downloading PRISM data:
# Normals based on LATEST 30-year period: currently 1981-2010 ? (1991-2020 should be)
# Can be downloaded in 4km/800m resolution for given day/month/vector of days/months/annual averages
# Download March 14 30-year average precip. Note the year is ignored
get_prism_normals('ppt', '4km', day = as.Date('2025-03-14'))
# Download the January - June 30-year averages at 4km resolution
get_prism_normals(type="tmean", resolution = "4km", mon = 1:6, keepZip = FALSE)
# Download the 30-year annual average precip and annual average temperature
get_prism_normals("ppt", "4km", annual = TRUE, keepZip = FALSE)
get_prism_normals("tmean", "4km", annual = TRUE, keepZip = FALSE)
# PRISM downloaded as zip files then unzipped
# If keepZip is TRUE: zip file remains on machine, else: automatically deleted

## Checking PRISM: 
prism_get_dl_dir()  # shows your download directory
list.files(prism_get_dl_dir())  # lists all downloaded files
# Downloaded 8 datasets (unzipped folders): 
# 3/14 ppt, Annual ppt, Jan - June temps, Annual temp
# 1 leftover zip file: PRISM_ppt_30yr_normal_4kmD1_0314_bil.zip

## Downloading daily, monthly, & annual data:
# Download daily average temperature from June 1 to June 14, 2013
get_prism_dailys(type = "tmean", minDate = "2013-06-01", maxDate = "2013-06-14", keepZip = FALSE)
# Download Jan average temperature data from 1982 to 2014
get_prism_monthlys(type = "tmean", year = 1982:2014, mon = 1, keepZip = FALSE)
# Download annual average precipitation for 2000 to 2015
get_prism_annual("ppt", years = 2000:2015, keepZip = FALSE)
# For daily data: need to give well formed date string in "YYYY-MM-DD"

## Interacting with PRISM archive & PRISM data:
# View all PRISM data you have downloaded: 
# Gives list of folder names (pd)
prism_archive_ls()
# Other files may need an absolute path (raster package)
# pd_to_file() returns absolute path
pd_to_file(prism_archive_ls())
# View normal name for product first (NOT file name)
pd_get_name(prism_archive_ls())

## Search for specific parameters, time steps, days, months, years, ranges of days/months/years
# Search for June 2013 daily data
pd_get_name(prism_archive_subset("tmean", "daily", mon = 6))
# Look for days between June 7 and June 10
pd_get_name(prism_archive_subset("tmean", "daily", minDate = "2013-06-07", maxDate = "2013-06-10"))


## Raster plots 
# Plot the January 30-year average temperatures
jmean <- prism_archive_subset("tmean", "monthly normals", mon = 1, resolution = "4km")
pd_image(jmean)

# Look at January temperature anomalies:
# Examine difference b/w Jan 2013 and Jan 30-year normals
library(raster)
# Knowing name of target file allows you to find them in all files that exist
# jnorm_name <- "PRISM_tmean_30yr_normal_4kmM2_01_bil"
# j2013_name <- "PRISM_tmean_stable_4kmM3_201301_bil"
# But we will use prism_archive_subset() to find the files we need to show:
# Getting January 30-year normals
jnorm <- prism_archive_subset("tmean", "monthly normals", mon = 1, resolution = "4km")
# Getting January 2013 
j2013 <- prism_archive_subset("tmean", "monthly", years = 2013, mon = 1)
# raster needs a full path, not the "short" prism data name
jnorm <- pd_to_file(jnorm)
j2013 <- pd_to_file(j2013)
# Load the rasters.
jnorm_rast <- raster(jnorm)
j2013_rast <- raster(j2013)
# Now we can do simple subtraction to get the anomaly by subtracting 2014 from the 30 year normal map
anomCalc <- function(x, y) {return(x - y)}
anom_rast <- raster::overlay(j2013_rast,jnorm_rast,fun = anomCalc)
plot(anom_rast)
# Plot shows Jan 2013 was warmer than average over last 30 years

## Single grid cell plot
## Visualizing a single point across multiple PRISM data files (slice)
## Takes a set of rasters, create a raster::stack, extract data at point, create ggplot2 object
library(ggplot2)
# Making a plot of Jan temperatures in Boulder b/w 1982 & 2014
# Data already exist in the prism dl dir
boulder <- c(-105.2797, 40.0176)
# prism_archive_subset() will return prism data that matches the specified variable
to_slice <- prism_archive_subset("tmean", "monthly", mon = 1)
p <- pd_plot_slice(to_slice, boulder)
# Add a linear average and title
p + 
  stat_smooth(method="lm", se = FALSE) + 
  theme_bw() + 
  ggtitle("Average January temperature in Boulder, CO 1982-2014")
#> `geom_smooth()` using formula = 'y ~ x'

## Leaflet map (make JS maps & can be hosted on Rpubs/etc. or your own site)
library(leaflet)
# Plotting 30-year normal for annual temperature
# 30-year normal average temperature have already been downloaded for 
norm <- prism_archive_subset("tmean", "annual normals", resolution = "4km")
rast <- raster(pd_to_file(norm))
# Create color palette
pal <- colorNumeric(c("#0000FF", "#FFFF00", "#FF0000"), values(rast),na.color = "transparent")
# Plot
leaflet() %>% 
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>% 
  addRasterImage(rast, colors = pal, opacity=.65) %>% 
  addLegend(pal = pal, values = values(rast), title = "Deg C")