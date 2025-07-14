## Load Michael landscape variables and complete correlation analysis

## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

## Creating huge spreadsheet of EVs + thermal sensitivities of 72 sites (no 1 AREMP site anymore)
library(tidyverse)
#--------------------------------------------------------------------------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------------------------------------------------------------------------
# Load required libraries
library(corrplot)
library(Hmisc) # produces correlation matrix
library(car)
# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics) # informative figure
library(gridExtra)
library(psych)
# Spearman's correlation analysis: default is alternative is two-sided, confidence level is 95%
#-------------------------------------------------------------------------------------------------------------------------------------------------
## Create full correlation matrix dataset - all 40 landscape variables + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and all landscape EVs
TSandAllEVsData <- TSAndEVs2021[, c(RVsTS2021, EVs2021)]
# Compute full correlation matrix using Spearman's 
fullCorrMatrix <- rcorr(as.matrix(TSandAllEVsData), type = "spearman")
# Save fullCorrMatrix of all 40 landscape EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(fullCorrMatrix, "results/2021/correlation/RDS/fullCorrMatrix.RDS")

# Save correlation matrix as CSV with names in first column
fullCorrMatrixDF <- as.data.frame(fullCorrMatrix$r)
fullCorrMatrixDF$Variable <- rownames(fullCorrMatrixDF)
fullCorrMatrixDF <- fullCorrMatrixDF[, c("Variable", setdiff(names(fullCorrMatrixDF), "Variable"))]
# Correlation coefficients
write_csv(fullCorrMatrixDF, "results/2021/correlation/coefficients/fullCorrMatrixCoeffs.csv")
# Save correlation matrix as CSV with names in first column
fullCorrMatrixPVals <- as.data.frame(fullCorrMatrix$P)
fullCorrMatrixPVals$Variable <- rownames(fullCorrMatrixPVals)
fullCorrMatrixPVals <- fullCorrMatrixPVals[, c("Variable", setdiff(names(fullCorrMatrixPVals), "Variable"))]
# P values
write_csv(fullCorrMatrixPVals, "results/2021/correlation/pvalues/fullCorrMatrixPVals.csv")
#--------------------------------------------------------------------------------------------------------------------------------------------------
### EVs2021NHD + RVsTS2021
## Create NHD correlation matrix dataset - 4 landscape variables + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and 4 landscape EVs
TSandNHDEVsData <- TSAndEVs2021[, c(RVsTS2021, EVs2021NHD)]
# Compute NHD correlation matrix using Spearman's 
NHDCorrMatrix <- rcorr(as.matrix(TSandNHDEVsData), type = "spearman")
# Save NHDCorrMatrix of 4 landscape EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(NHDCorrMatrix, "results/2021/correlation/RDS/NHDCorrMatrix.RDS")

# Save correlation matrix as CSV with names in first column
NHDCorrMatrixDF <- as.data.frame(NHDCorrMatrix$r)
NHDCorrMatrixDF$Variable <- rownames(NHDCorrMatrixDF)
NHDCorrMatrixDF <- NHDCorrMatrixDF[, c("Variable", setdiff(names(NHDCorrMatrixDF), "Variable"))]
# Correlation coefficients
write_csv(NHDCorrMatrixDF, "results/2021/correlation/coefficients/NHDCorrMatrixCoeffs.csv")
# Save correlation matrix as CSV with names in first column
NHDCorrMatrixPVals <- as.data.frame(NHDCorrMatrix$P)
NHDCorrMatrixPVals$Variable <- rownames(NHDCorrMatrixPVals)
NHDCorrMatrixPVals <- NHDCorrMatrixPVals[, c("Variable", setdiff(names(NHDCorrMatrixPVals), "Variable"))]
# P values
write_csv(NHDCorrMatrixPVals, "results/2021/correlation/pvalues/NHDCorrMatrixPVals.csv")
#--------------------------------------------------------------------------------------------------------------------------------------------------
### EVs2021Upstream + RVsTS2021
## Create upstream correlation matrix dataset - 12 h2o landscape variables + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and 12 h2o landscape EVs
TSandUpstreamEVsData <- TSAndEVs2021[, c(RVsTS2021, EVs2021Upstream)]
# Compute upstream correlation matrix using Spearman's 
UpstreamCorrMatrix <- rcorr(as.matrix(TSandUpstreamEVsData), type = "spearman")
# Save UpstreamCorrMatrix of 12 h2o landscape EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(UpstreamCorrMatrix, "results/2021/correlation/RDS/UpstreamCorrMatrix.RDS")

# Save correlation matrix as CSV with names in first column
UpstreamCorrMatrixDF <- as.data.frame(UpstreamCorrMatrix$r)
UpstreamCorrMatrixDF$Variable <- rownames(UpstreamCorrMatrixDF)
UpstreamCorrMatrixDF <- UpstreamCorrMatrixDF[, c("Variable", setdiff(names(UpstreamCorrMatrixDF), "Variable"))]
# Correlation coefficients
write_csv(UpstreamCorrMatrixDF, "results/2021/correlation/coefficients/UpstreamCorrMatrixCoeffs.csv")
# Save correlation matrix as CSV with names in first column
UpstreamCorrMatrixPVals <- as.data.frame(UpstreamCorrMatrix$P)
UpstreamCorrMatrixPVals$Variable <- rownames(UpstreamCorrMatrixPVals)
UpstreamCorrMatrixPVals <- UpstreamCorrMatrixPVals[, c("Variable", setdiff(names(UpstreamCorrMatrixPVals), "Variable"))]
# P values
write_csv(UpstreamCorrMatrixPVals, "results/2021/correlation/pvalues/UpstreamCorrMatrixPVals.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
### EVs2021Catchment + RVsTS2021
## Create catchment correlation matrix dataset - 9 landscape variables + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and 9 landscape EVs
TSandCatchmentEVsData <- TSAndEVs2021[, c(RVsTS2021, EVs2021Catchment)]
# Compute catchment correlation matrix using Spearman's 
CatchmentCorrMatrix <- rcorr(as.matrix(TSandCatchmentEVsData), type = "spearman")
# Save CatchmentCorrMatrix of 9 landscape EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(CatchmentCorrMatrix, "results/2021/correlation/RDS/CatchmentCorrMatrix.RDS")

# Save correlation matrix as CSV with names in first column
CatchmentCorrMatrixDF <- as.data.frame(CatchmentCorrMatrix$r)
CatchmentCorrMatrixDF$Variable <- rownames(CatchmentCorrMatrixDF)
CatchmentCorrMatrixDF <- CatchmentCorrMatrixDF[, c("Variable", setdiff(names(CatchmentCorrMatrixDF), "Variable"))]
# Correlation coefficients
write_csv(CatchmentCorrMatrixDF, "results/2021/correlation/coefficients/CatchmentCorrMatrixCoeffs.csv")
# Save correlation matrix as CSV with names in first column
CatchmentCorrMatrixPVals <- as.data.frame(CatchmentCorrMatrix$P)
CatchmentCorrMatrixPVals$Variable <- rownames(CatchmentCorrMatrixPVals)
CatchmentCorrMatrixPVals <- CatchmentCorrMatrixPVals[, c("Variable", setdiff(names(CatchmentCorrMatrixPVals), "Variable"))]
# P values
write_csv(CatchmentCorrMatrixPVals, "results/2021/correlation/pvalues/CatchmentCorrMatrixPVals.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
### EVs2021Buffer + RVsTS2021
## Create buffer correlation matrix dataset - 9 landscape variables + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and 9 landscape EVs
TSandBufferEVsData <- TSAndEVs2021[, c(RVsTS2021, EVs2021Buffer)]
# Compute buffer correlation matrix using Spearman's 
BufferCorrMatrix <- rcorr(as.matrix(TSandBufferEVsData), type = "spearman")
# Save BufferCorrMatrix of 9 landscape EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(BufferCorrMatrix, "results/2021/correlation/RDS/BufferCorrMatrix.RDS")

# Save correlation matrix as CSV with names in first column
BufferCorrMatrixDF <- as.data.frame(BufferCorrMatrix$r)
BufferCorrMatrixDF$Variable <- rownames(BufferCorrMatrixDF)
BufferCorrMatrixDF <- BufferCorrMatrixDF[, c("Variable", setdiff(names(BufferCorrMatrixDF), "Variable"))]
# Correlation coefficients
write_csv(BufferCorrMatrixDF, "results/2021/correlation/coefficients/BufferCorrMatrixCoeffs.csv")
# Save correlation matrix as CSV with names in first column
BufferCorrMatrixPVals <- as.data.frame(BufferCorrMatrix$P)
BufferCorrMatrixPVals$Variable <- rownames(BufferCorrMatrixPVals)
BufferCorrMatrixPVals <- BufferCorrMatrixPVals[, c("Variable", setdiff(names(BufferCorrMatrixPVals), "Variable"))]
# P values
write_csv(BufferCorrMatrixPVals, "results/2021/correlation/pvalues/BufferCorrMatrixPVals.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
### EVs2021Climate + RVsTS2021
## Create climate correlation matrix dataset - 6 climate variables + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and 6 climate EVs
TSandClimateEVsData <- TSAndEVs2021[, c(RVsTS2021, EVs2021Climate)]
# Compute climate correlation matrix using Spearman's 
ClimateCorrMatrix <- rcorr(as.matrix(TSandClimateEVsData), type = "spearman")
# Save ClimateCorrMatrix of 6 climate EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(ClimateCorrMatrix, "results/2021/correlation/RDS/ClimateCorrMatrix.RDS")

# Save correlation matrix as CSV with names in first column
ClimateCorrMatrixDF <- as.data.frame(ClimateCorrMatrix$r)
ClimateCorrMatrixDF$Variable <- rownames(ClimateCorrMatrixDF)
ClimateCorrMatrixDF <- ClimateCorrMatrixDF[, c("Variable", setdiff(names(ClimateCorrMatrixDF), "Variable"))]
# Correlation coefficients
write_csv(ClimateCorrMatrixDF, "results/2021/correlation/coefficients/ClimateCorrMatrixCoeffs.csv")
# Save correlation matrix as CSV with names in first column
ClimateCorrMatrixPVals <- as.data.frame(ClimateCorrMatrix$P)
ClimateCorrMatrixPVals$Variable <- rownames(ClimateCorrMatrixPVals)
ClimateCorrMatrixPVals <- ClimateCorrMatrixPVals[, c("Variable", setdiff(names(ClimateCorrMatrixPVals), "Variable"))]
# P values
write_csv(ClimateCorrMatrixPVals, "results/2021/correlation/pvalues/ClimateCorrMatrixPVals.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
library(reshape2)
### Creating a function to create informative summary for each correlation matrix
analyzeCorrMatrix <- function(corrMatrix, outputFile) {
    # Extract coefficients & p values from correlation matrix
    coefficients <- corrMatrix$r 
    pValues <- corrMatrix$P
    # Reshape data to tall, normal structure
    coefficientsLong <- melt(coefficients, varnames = c("Variable 1", "Variable 2"), value.name = "Correlation Coefficient")
    pValuesLong <- melt(pValues, varnames = c("Variable 1", "Variable 2"), value.name = "p Value")
    # Combine correlation coefficient & p value
    table <- merge(coefficientsLong, pValuesLong, by = c("Variable 1", "Variable 2")) 
    # Remove diagonal (self-correlation) 
    table <- table[table$`Variable 1` != table$`Variable 2`, ]
    # Remove duplicate - not a pair anymore
    table <- table[!duplicated(t(apply(table[,1:2], 1, sort))), ] 
    # Find absolute value of correlation coefficient
    table$`Absolute Value of Correlation` <- abs(table$`Correlation Coefficient`)
    # Categorize correlation strength
    table$`Correlation Strength` <- cut(table$`Absolute Value of Correlation`, breaks = c(0, 0.19, 0.39, 0.59, 0.79, 1.00), labels = c("Very Weak", "Weak", "Moderate", "Strong", "Very Strong"), include.lowest = TRUE)
    # Find direction of correlation
    table$Direction <- ifelse(table$`Correlation Coefficient` > 0, "Positive", "Negative")
    # Sort by highest significance & highest strength
    table <- table[order(table$`p Value`, -table$`Absolute Value of Correlation`), ]
    # Save table
    write_csv(table, outputFile)
    return(table)
}
# Run analyzeCorrMatrix on all correlation matrices
FullInfoSummary <- analyzeCorrMatrix(fullCorrMatrix, "results/2021/correlation/summary/fullInfoSummary.csv")
NHDInfoSummary <- analyzeCorrMatrix(NHDCorrMatrix, "results/2021/correlation/summary/NHDInfoSummary.csv")
UpstreamInfoSummary <- analyzeCorrMatrix(UpstreamCorrMatrix, "results/2021/correlation/summary/UpstreamInfoSummary.csv")
CatchmentInfoSummary <- analyzeCorrMatrix(CatchmentCorrMatrix, "results/2021/correlation/summary/CatchmentInfoSummary.csv")
BufferInfoSummary <- analyzeCorrMatrix(BufferCorrMatrix, "results/2021/correlation/summary/BufferInfoSummary.csv")
ClimateInfoSummary <- analyzeCorrMatrix(ClimateCorrMatrix, "results/2021/correlation/summary/ClimateInfoSummary.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
# Filter your full info summary for only thermal sensivity correlations w/ every landscape EV
TSOnlyInfoSummary <- FullInfoSummary[FullInfoSummary$`Variable 1` == "thermalSensitivity" | FullInfoSummary$`Variable 2` == "thermalSensitivity", ]
# Save it
write_csv(TSOnlyInfoSummary, "results/2021/correlation/summary/TSOnlyInfoSummary.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
### Correlation plots divided based on scale
