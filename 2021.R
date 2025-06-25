# Cleaning data for 2021 Clackamas Data 

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# WANT: data frame of Logger - Series # - Date - Time - Temp in C 
# Sheets also have graphs btw
# Note: 76 sites/sheet & 4 USGS sites/sheets

library(readxl)  
file2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/ClackData2021.xlsx"
# Get 2021 Clackamas Data Sheet Names
sites2021 <- excel_sheets(file2021)
# Print 2021 Clackamas Data Sheet Names
sites2021

file.exists(file2021)
print(file2021)

print(sites2021)
length(sites2021)
# Read all sheets in 2021 Clackamas Data to list of data frame objects (each site/sheet is a df object)
listOfSites <- lapply(sites2021, function(x) {as.data.frame(read_excel(file2021, sheet = x))})
# Rename elements in list to be the names of the sheet names
names(listOfSites) <- sites2021


# CHECKING: 
# Checking sheets read
length(listOfSites)
length(sites2021)

# Check names of list elements
names(listOfSites)

# Check structure of list
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