## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

## Creating huge spreadsheet of EVs + thermal sensitivities of 72 sites (no 1 AREMP site anymore)
library(tidyverse)
# Loading RDS structures - using R in VS Code is finnicky
SortedDaymetTSAndEVs2021 <- readRDS("results/2021/RDS/SortedDaymetTSAndEVs2021.RDS")
#-------------------------------------------------------------------------------------------------------------------------------------------------
## Perform Spearman's correlation analysis on 2021 EVs

# Separate potential RVs
RVs2021 <- c("thermalSensitivity", "meanStreamTemp", "meanAirTemp", "minStreamTemp", "maxStreamTemp", "minAirTemp", "maxAirTemp", "rangeStreamTemp", "rangeAirTemp")
RVsTS2021 <- c("thermalSensitivity", "meanStreamTemp", "meanAirTemp")

# Define EVs/landscape variables from Michael - no Solar though
EVs2021 <- c("SLOPE", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip")
# Daymet EVs
daymetEVs2021 <- c("daymet_dayl (s)", "daymet_prcp (mm/day)", "daymet_srad (W/m^2)", "daymet_swe (kg/m^2)", "daymet_tmax (deg c)", "daymet_tmin (deg c)", "daymet_vp (Pa)")
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
## Create full correlation matrix dataset - all 40 landscape variables (no Solar) + daymet EVs + thermal sensitivity, mean air temperature, mean stream temperature
# Join TS RVs and all landscape EVs
TSandAllEVsDaymetData <- SortedDaymetTSAndEVs2021[, c(RVsTS2021, EVs2021, daymetEVs2021)]
# Compute full correlation matrix using Spearman's 
fullCorrMatrixDaymet <- rcorr(as.matrix(TSandAllEVsDaymetData), type = "spearman")
# Save fullCorrMatrix of all 40 landscape EVs (no Solar) + daymet EVs + thermal sensitivity, mean air temperature, mean stream temperature: RDS & csv
saveRDS(fullCorrMatrixDaymet, "results/2021/correlation/RDS/fullCorrMatrixDaymet.RDS")

# Save correlation matrix as CSV with names in first column
fullCorrMatrixDaymetDF <- as.data.frame(fullCorrMatrixDaymet$r)
fullCorrMatrixDaymetDF$Variable <- rownames(fullCorrMatrixDaymetDF)
fullCorrMatrixDaymetDF <- fullCorrMatrixDaymetDF[, c("Variable", setdiff(names(fullCorrMatrixDaymetDF), "Variable"))]
# Correlation coefficients
write_csv(fullCorrMatrixDaymetDF, "results/2021/correlation/coefficients/fullCorrMatrixDaymetCoeffs.csv")
# Save correlation matrix as CSV with names in first column
fullCorrMatrixDaymetPVals <- as.data.frame(fullCorrMatrixDaymet$P)
fullCorrMatrixDaymetPVals$Variable <- rownames(fullCorrMatrixDaymetPVals)
fullCorrMatrixDaymetPVals <- fullCorrMatrixDaymetPVals[, c("Variable", setdiff(names(fullCorrMatrixDaymetPVals), "Variable"))]
# P values
write_csv(fullCorrMatrixDaymetPVals, "results/2021/correlation/pvalues/fullCorrMatrixDaymetPVals.csv")
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
FullInfoDaymetSummary <- analyzeCorrMatrix(fullCorrMatrixDaymet, "results/2021/correlation/summary/fullInfoDaymetSummary.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
# Filter your full info summary for only thermal sensivity correlations w/ every landscape EV
TSDaymetOnlyInfoSummary <- FullInfoDaymetSummary[FullInfoDaymetSummary$`Variable 1` == "thermalSensitivity" | FullInfoDaymetSummary$`Variable 2` == "thermalSensitivity", ]
# Save it
write_csv(TSDaymetOnlyInfoSummary, "results/2021/correlation/summary/TSDaymetOnlyInfoSummary.csv")
#---------------------------------------------------------------------------------------------------------------------------------------------------
