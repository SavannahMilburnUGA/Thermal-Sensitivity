## NEW - Load Michael landscape variables and complete correlation analysis
## Load Michael landscape variables (EVs)

## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

## Creating huge spreadsheet of EVs + thermal sensitivities of 72 sites (no 1 AREMP site anymore)
library(tidyverse)

# Load thermalSensitivities2021 RDS
thermalSensitivities2021 <- readRDS("results/2021/RDS/thermalSensitivities2021.RDS")
# Check
nrow(thermalSensitivities2021) # 73 sites 

# Load CRB EVs - from Michael
library(readxl)
fileEVs2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/EVs/sites_attributes_2021.xlsx"
# Check for file exists
file.exists(fileEVs2021)
print(fileEVs2021)
ClackEVs2021 <- read_excel(fileEVs2021)
# Check
head(ClackEVs2021)

# Rename 4 USGS sites so can join ClackEVs2021 + thermalSensitivities2021
ClackEVs2021 <- ClackEVs2021 %>%
  mutate(Filename = case_when(
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - CARTER BRIDGE" ~ "USGS 1",
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - ESTACADA" ~ "USGS 2", 
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - OREGON CITY" ~ "USGS 3",
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - FISH CREEK" ~ "USGS 4",
    TRUE ~ Filename
  ))
# Check
View(ClackEVs2021) # USGS 1-4

# Remove 7 unneeded sites from ClackEVs2021 to make 73 sites 
remove <- c("10598088", "20733169", "USGS 3", "10361309", "10931479", "11007911", "11007937", "20539817")
ClackEVs2021 <- ClackEVs2021 %>%
  filter(!Filename %in% remove)
# Check
nrow(ClackEVs2021) # Should be 73

# Join thermal sensitivities + EVs - huge spreadsheet for correlation analysis
TSAndEVs2021 <- thermalSensitivities2021 %>%
  left_join(ClackEVs2021, by = c("site" = "Filename")) %>%
  filter(!is.na(thermalSensitivity))
# Check 
nrow(TSAndEVs2021) # 73

# Michael sites_attributes_2021.xlsx does NOT have AREMP sites SO:
# Remove 1 AREMP site: deploy ORCUB001IN02
TSAndEVs2021 <- TSAndEVs2021 %>%
    filter(!site == "ORCUB001IN02")
# Check 
nrow(TSAndEVs2021) # 72
View(TSAndEVs2021)

# Save TSAndEVs file - only have 69 sites + 3 USGS sites now (NO 1 AREMP site)
# Huge spreadsheet Dr. Chang was wanting
saveRDS(TSAndEVs2021, "results/2021/RDS/TSAndEVs2021.RDS")
write.csv(TSAndEVs2021, "results/2021/TSAndEVs2021.csv", row.names=FALSE)
#-------------------------------------------------------------------------------------------------------------------------------------------------
## Perform Spearman's correlation analysis on 2021 EVs

# Separate potential RVs
RVs2021 <- c("thermalSensitivity", "meanStreamTemp", "meanAirTemp", "minStreamTemp", "maxStreamTemp", "minAirTemp", "maxAirTemp", "rangeStreamTemp", "rangeAirTemp")
RVsTS2021 <- c("thermalSensitivity", "meanStreamTemp", "meanAirTemp")

# Define EVs/landscape variables from Michael 
EVs2021 <- c("SLOPE", "Solar", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip")
# EVs separated based on scales: upstream, catchment, buffer
EVs2021NHD <- c("SLOPE", "Solar", "Elev", "BFI")
EVs2021Upstream <- c("h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2")
EVs2021Catchment <- c("BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_")
EVs2021Buffer <- c("DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf")
EVs2021Climate <- c("MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip")

# Load required libraries
library(corrplot)
library(Hmisc) # produces correlation matrix
library(car)
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) # informative figure
library(gridExtra)
library(psych)

## Create full correlation matrix dataset - all 40 landscape variables + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and all landscape EVs
TSandAllEVsData <- TSAndEVs2021[, c(RVsTS2021, EVs2021)]
# Check
View(TSandAllEVsData)
# Compute full correlation matrix using Spearman's 
fullCorrMatrix <- rcorr(as.matrix(TSandAllEVsData), type = "spearman")
# Save fullCorrMatrix of all 40 landscape EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(fullCorrMatrix, "results/2021/correlation/RDS/fullCorrMatrix.RDS")
# Save correlation matrix as CSV with names in first column
fullCorrMatrixDF <- as.data.frame(fullCorrMatrix$r)
fullCorrMatrixDF$LandscapeEV <- rownames(fullCorrMatrixDF)
fullCorrMatrixDF <- fullCorrMatrixDF[, c("Variable", setdiff(names(fullCorrMatrixDF), "LandscapeEV"))]
# Correlation coefficients
fullCorrMatrix <- fullCorrMatrix
write_csv(fullCorrMatrixDF, "results/2021/correlation/coefficients/fullCorrMatrixCoeffs.csv")
# p-values
write_csv(as.data.frame(fullCorrMatrix$P), "results/2021/correlation/pvalues/fullCorrMatrixPVals.csv")

## HERE 
# alternative is two-sided, confidence level is 95%
## Generate correlation matrix using Spearman's correlation analysis






# OLD VERIFICATION METHODS AFTER DOING CORRELATION --------------------------------------------------------------------------------------------
cat("\n=== VERIFICATION METHODS ===\n")

# Method A: Compare with base R cor() function
cat("Verification A: Compare with base R cor() function\n")
base_r_corr <- cor(corrDataClean, method = "spearman", use = "complete.obs")
thermal_base <- base_r_corr["thermalSensitivity", ]

# Check if results match
matches <- all.equal(TSCorrCoeff, thermal_base)
cat("Hmisc and base R results match:", is.logical(matches) && matches, "\n")

# Method B: Manual calculation for one variable (as example)
cat("\nVerification B: Manual Spearman calculation example\n")
# Pick first landscape variable for manual verification
test_var <- thermal_results$variable[1]
cat("Testing variable:", test_var, "\n")

# Manual Spearman calculation
x_ranks <- rank(corrDataClean$thermalSensitivity)
y_ranks <- rank(corrDataClean[[test_var]])
manual_spearman <- cor(x_ranks, y_ranks)

auto_spearman <- TSCorrCoeff[test_var]
cat("Manual Spearman:", round(manual_spearman, 6), "\n")
cat("Automatic Spearman:", round(auto_spearman, 6), "\n")
cat("Match:", round(manual_spearman, 6) == round(auto_spearman, 6), "\n")

# Method C: Check with psych package
psych_result <- corr.test(corrDataClean, method = "spearman")
psych_thermal <- psych_result$r["thermalSensitivity", ]
cat("\nVerification C: psych package results match:", 
    all.equal(TSCorrCoeff, psych_thermal), "\n")