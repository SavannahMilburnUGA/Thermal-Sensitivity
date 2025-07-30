## Load 39 Michael landscape EVs, DAYMET, orientation w/ sorted TS and complete correlation analysis
# No Solar from Michael 

## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

## Creating huge spreadsheet of 39 EVs + thermal sensitivities + DAYMET + orienatation of 72 sorted sites (no 1 AREMP site anymore)
library(tidyverse)
#--------------------------------------------------------------------------------------------------------------------------------------------------
# Load SortedO_D_TS_EVs2021 RDS
SortedO_D_TS_EVs2021 <- readRDS("results/2021/RDS/SortedO_D_TS_EVs2021.RDS")
nrow(SortedO_D_TS_EVs2021) # 72 sites 
length(SortedO_D_TS_EVs2021) # 69 columns
#-------------------------------------------------------------------------------------------------------------------------------------------------
## Perform Spearman's correlation analysis on 2021 EVs, DAYMET, orientation

# Separate potential RVs
RVs2021 <- c("thermalSensitivity", "meanStreamTemp", "meanAirTemp", "minStreamTemp", "maxStreamTemp", "minAirTemp", "maxAirTemp", "rangeStreamTemp", "rangeAirTemp")
RVsTS2021 <- c("thermalSensitivity", "meanStreamTemp", "meanAirTemp")

names(SortedO_D_TS_EVs2021)

# Define 39 EVs/landscape variables from Michael - no Solar + 4 DAYMET EVs + Azimuth & AbsAzimuth
EVs2021 <- c("SLOPE", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip", "daymetDayl", "daymetPrcp", "daymetSRad", "daymetVP", "Azimuth", "AbsAzimuth")

#-------------------------------------------------------------------------------------------------------------------------------------------------
# Load required libraries
library(corrplot)
library(Hmisc) # produces correlation matrix
library(car)
library(PerformanceAnalytics) # informative figure
library(gridExtra)
library(psych)
# Spearman's correlation analysis: default is alternative is two-sided, confidence level is 95%
#-------------------------------------------------------------------------------------------------------------------------------------------------
## Create full 39_ODT correlation matrix dataset - 39 landscape variables + TS, mean AT, mean ST + 4 DAYMET Evs + Azimuth & AbsAzimuth
# Join TS RVs and 39 landscape EVs + 4 DAYMET EVs + 2 Orientation EVs
TSand39EVs_ODT_Data <- SortedO_D_TS_EVs2021[, c(RVsTS2021, EVs2021)]
# Compute full 39 landscape EV + 4 DAYMET EVs + 2 orientatino EVs correlation matrix using Spearman's 
full39_ODT_CorrMatrix <- rcorr(as.matrix(TSand39EVs_ODT_Data), type = "spearman")
# Save full39_ODT_CorrMatrix of 39 landscape EVs + thermal sensitivity, mean air temperature, mean stream temperature + 4 DAYMET EVs + 2 Orientation EVs: RDS & csv
saveRDS(full39_ODT_CorrMatrix, "results/2021/correlation/RDS/full39_ODT_CorrMatrix.RDS")

# Save full39_ODT_CorrMatrix as CSV with names in first column
full39_ODT_CorrMatrixDF <- as.data.frame(full39_ODT_CorrMatrix$r)
full39_ODT_CorrMatrixDF$Variable <- rownames(full39_ODT_CorrMatrixDF)
full39_ODT_CorrMatrixDF <- full39_ODT_CorrMatrixDF[, c("Variable", setdiff(names(full39_ODT_CorrMatrixDF), "Variable"))]
# Correlation coefficients
write_csv(full39_ODT_CorrMatrixDF, "results/2021/correlation/coefficients/full39_ODT_CorrMatrixCoeffs.csv")

# Save correlation matrix as CSV with names in first column
full39_ODT_CorrMatrixPVals <- as.data.frame(full39_ODT_CorrMatrix$P)
full39_ODT_CorrMatrixPVals$Variable <- rownames(full39_ODT_CorrMatrixPVals)
full39_ODT_CorrMatrixPVals <- full39_ODT_CorrMatrixPVals[, c("Variable", setdiff(names(full39_ODT_CorrMatrixPVals), "Variable"))]
# P values
write_csv(full39_ODT_CorrMatrixPVals, "results/2021/correlation/pvalues/full39_ODT_CorrMatrixPVals.csv")
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
# Run analyzeCorrMatrix on full39_ODT corr matric
full39_ODT_InfoSummary <- analyzeCorrMatrix(full39_ODT_CorrMatrix, "results/2021/correlation/summary/full39_ODT_InfoSummary.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
# Filter your full info summary for only thermal sensivity correlations w/ 39 landscape EVs + 4 DAYMET EVs + 2 Orientation EVs
TSOnly_39_ODT_InfoSummary <- full39_ODT_InfoSummary[full39_ODT_InfoSummary$`Variable 1` == "thermalSensitivity" | full39_ODT_InfoSummary$`Variable 2` == "thermalSensitivity", ]
# Save it
write_csv(TSOnly_39_ODT_InfoSummary, "results/2021/correlation/summary/TSOnly_39_ODT_InfoSummary.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------

