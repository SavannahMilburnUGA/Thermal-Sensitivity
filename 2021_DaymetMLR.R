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
## Performing 2021_MLR.R on Daymet + 39 landscape EVs (no Solar) + TS
# Load packages
library(readr) # for write_csv
library(lm.beta) # for standardized beta coefficients
library(car) # VIFs
library(leaps) # selecting best MLR on highest adjusted R squared
#-----------------------------------------------------------------------------------------------------------------------------------------------
## Function to generate model info summary
createInfoSummary <- function(model, betaModel, corrMatrix, modelName) {
    # Summary of regular lm model
    modelSummary <- summary(model)
    # F p value
    fPValue <- pf(modelSummary$fstatistic[1], modelSummary$fstatistic[2], modelSummary$fstatistic[3], lower.tail = FALSE)
    # Find standardized beta coefficients using lm beta model
    stdCoeffs <- betaModel$standardized.coefficients
    stdCoeffs <- stdCoeffs[!is.na(stdCoeffs)] # Remove intercept
    # Find normal coefficients
    coeffTable <- modelSummary$coefficients[-1, ]
    # Find # of significant landscape EVs
    numSigEVs <- sum(coeffTable[, "Pr(>|t|)"] < 0.05)
    EVNames <- names(stdCoeffs)
    # Find correlation w/ thermal sensitivity
    TSCorr <- corrMatrix$r["thermalSensitivity", EVNames]
    # Create actual data frame object
    infoSummary <- data.frame(
        `Model Name` = rep(modelName, length(EVNames)), 
        `Landscape EV` = EVNames, 
        Estimate = coeffTable[EVNames, "Estimate"], 
        `Standardized Coefficient` = stdCoeffs,
        `Std Error` = coeffTable[EVNames, "Std. Error"], 
        `t Value` = coeffTable[EVNames, "t value"], 
        `p Value` = coeffTable[EVNames, "Pr(>|t|)"],
        Significance = ifelse(coeffTable[EVNames, "Pr(>|t|)"] < 0.001, "***",
                             ifelse(coeffTable[EVNames, "Pr(>|t|)"] < 0.01, "**",
                                   ifelse(coeffTable[EVNames, "Pr(>|t|)"] < 0.05, "*",
                                         ifelse(coeffTable[EVNames, "Pr(>|t|)"] < 0.1, ".", "")))),
        `Correlation w/ thermal sensitivity` = TSCorr, 
        `R Squared` = rep(modelSummary$r.squared, length(EVNames)), 
        `Adjusted R Squared` = rep(modelSummary$adj.r.squared, length(EVNames)),
        `F statistic` = rep(modelSummary$fstatistic[1], length(EVNames)),  
        `F-p Value` = rep(fPValue, length(EVNames)), 
        `# of Significant EVs` = rep(numSigEVs, length(EVNames)), 
        `Total # of EVs` = rep(length(EVNames), length(EVNames)), 
        stringsAsFactors = FALSE
    )
    return(infoSummary)
}
#---------------------------------------------------------------------------------------------------------------------------------------------------------
## Function to find correlation coefficients above the correlation cut off: if yes - remove higher VIF. If VIFs are similar, remove lower adjusted R squared
findCorrelations <- function(corrMatrix, corrCutOff, inputEVs, VIFs, data) {
    # Find correlations above the correlation cut off
    cutOffPairs <- which(abs(corrMatrix) >= corrCutOff & corrMatrix != 1, arr.ind = TRUE)
    
    # Correlations that are higher than cut off found    
    if(nrow(cutOffPairs) > 0) {
        # Remove duplicates - self correlation
        noDuplicates <- cutOffPairs[cutOffPairs[,1] < cutOffPairs[,2], , drop = FALSE]
        
        # Keep track of landscape EVs to remove
        removedVars <- c()
        # Num of correlation pairs w/ correlation coefficients above correlation cut off
        cat(sprintf("Found %d variable pairs with |correlation| >= %.1f\n", nrow(noDuplicates), corrCutOff))
        
        # Iterate over correlation coefficients
        for(i in 1:nrow(noDuplicates)) {
            var1 <- rownames(corrMatrix)[noDuplicates[i,1]]
            var2 <- colnames(corrMatrix)[noDuplicates[i,2]]
            
            # Skip if either variable already marked for removal
            if(var1 %in% removedVars || var2 %in% removedVars) next
            
            cat(sprintf("\nHigh correlation pair: %s & %s (r = %.3f)\n", 
                        var1, var2, corrMatrix[var1, var2]))
            
            # Get VIFs for both variables
            var1VIF <- VIFs[var1]
            var2VIF <- VIFs[var2]
            
            # Get individual adjusted R^2
            var1AdjRSquared <- summary(lm(thermalSensitivity ~ get(var1), data = data))$adj.r.squared
            var2AdjRSquared <- summary(lm(thermalSensitivity ~ get(var2), data = data))$adj.r.squared
            
            cat(sprintf("  %s: VIF = %.2f, Adj R² = %.3f\n", var1, var1VIF, var1AdjRSquared))
            cat(sprintf("  %s: VIF = %.2f, Adj R² = %.3f\n", var2, var2VIF, var2AdjRSquared))
            # Find difference in 2 EV VIFs
            VIFDiff <- abs(var1VIF - var2VIF)
            # VIFs are not very similar so remove EV with higher VIF
            if(VIFDiff > 2) {
                removeVar <- ifelse(var1VIF > var2VIF, var1, var2)
            } else {
                # VIFs are similar so remove EV with lower adjusted R squared
                removeVar <- ifelse(var1AdjRSquared < var2AdjRSquared, var1, var2)
            }
            
            removedVars <- c(removedVars, removeVar)
            cat(sprintf("Removing %s\n", removeVar))
        }
        
        # Create final variable list
        finalEVs <- inputEVs[!inputEVs %in% removedVars]
        
        cat(sprintf("\nRemoved %d variables due to high correlations: %s\n", 
                    length(removedVars), paste(removedVars, collapse = ", ")))
    } else {
        finalEVs <- inputEVs
        cat("No high correlations found\n")
    }
    
    # Return the filtered variable list
    return(list(
        finalEVs = finalEVs,
        removedEVs = if(exists("removedVars")) removedVars else c(),
        numRemovedEVs = if(exists("removedVars")) length(removedVars) else 0
    ))
}
#--------------------------------------------------------------------------------------------------------------------------------------------------------
# Define 39 EVs/landscape variables from Michael - remove Solar as an EV + add DAYMET EVs (dayl, srad, vp)
EVsAndDaymet2021 <- c("SLOPE", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip", "daymet_dayl (s)", "daymet_srad (W/m^2)", "daymet_vp (Pa)")
# SLOPE, Elev, BFI, h2oDevelop, h2oLakesPe, h2oAgricul, h2oBurnPer, h2oRdDens, h2oHiCascP, h2oWetland, h2oVegCov, h2oVegHt, Forest21, Shrub21, h2oKm2, BurnRCA, AgricultRC, WetlandsRC, LakesRCA, HiCascRCA, DevelopRCA, RoadsRCA, VegCover, VegHeight_, DevelopBuf, AgBuf, BurnBuf, WetlandBuf, LakesBuf, HiCascBuf, RoadsBuf, VegHtBuf, VegCovBuf, MeanMaxAir, MaxAir_C, Precip_mm, SumPrecip, MeanAirJJA, WetPrecip, daymet_dayl (s), daymet_srad (W/m^2), daymet_vp (Pa)
# Check main data frame
nrow(SortedDaymetTSAndEVs2021) # No 1 AREMP site - 72 sites
#------------------------------------------------------------------------------------------------------------------------------------------------
#1: BEFORE conducting MULTIVARIATE CORRELATIONS, TEST for MULTICOLLINEARITY among EVs by examining VIF factor & REMOVING any VALUES > 15
# Create linear regression w/ thermal sensitivity as RV & all landscape varbs as EVs (no Solar) + DAYMET EVs (dayl, srad, vp)
model1Daymet <- lm(thermalSensitivity ~ SLOPE + Elev + BFI + h2oDevelop + h2oLakesPe + h2oAgricul + h2oBurnPer + h2oRdDens + h2oHiCascP + h2oWetland + h2oVegCov + h2oVegHt + Forest21 + Shrub21 + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + LakesRCA + HiCascRCA + DevelopRCA + RoadsRCA + VegCover + VegHeight_ + DevelopBuf + AgBuf + BurnBuf + WetlandBuf + LakesBuf + HiCascBuf + RoadsBuf + VegHtBuf + VegCovBuf + MeanMaxAir + MaxAir_C + Precip_mm + SumPrecip + MeanAirJJA + WetPrecip + `daymet_dayl (s)` + `daymet_srad (W/m^2)` +  `daymet_vp (Pa)`, data = SortedDaymetTSAndEVs2021)

# Calculate VIF values
VIFsDaymet <- vif(model1Daymet)
# View VIF values run on model1
print("All landscape variables + DAYMET EVs with VIF values: ")
print(VIFsDaymet)
# Saving VIF values
write.csv(data.frame(Variable = names(VIFsDaymet), VIF = VIFsDaymet), "results/2021/MLR/daymet/model1DaymetVIFs.csv")
# All daymet EVs have higher VIFs... although srad is not as high as the others (~40)
# Finding EVs greater than VIF cutoff 
EVsDaymetHighVIF <- VIFsDaymet[VIFsDaymet > 15]
print("Landscape variables/DAYMET EVs with VIF > 15: ")
print(EVsDaymetHighVIF)
# Keep: AgBuf, BurnRCA, HiCascBuf, HiCascRCA, RoadsBuf, RoadsRCA, SLOPE, WetlandBuf, WetlandsRC, h2oHiCascP, h2oKm2, h2oLakesPe 
# DAYMET added h2okm2 & removed Shrub21, h2oRdDens, h2oWetland from prev MLR
# Take out EVsHighVIF from model1
# Create linear regression w/ thermal sensitivity as RV & landscape varbs & DAYMET varbs as EVs that have VIFS <= 15
model2Daymet <- lm(thermalSensitivity ~ AgBuf + BurnRCA + HiCascBuf + HiCascRCA + RoadsBuf + RoadsRCA + SLOPE + WetlandBuf + WetlandsRC + h2oHiCascP + h2oKm2 + h2oLakesPe, data = SortedDaymetTSAndEVs2021)
#------------------------------------------------------------------------------------------------------------------------------------------------
#2: Case where VARBS are ALIASED: run PAIRWISE LINEAR CORRELATIONS on ALL EVs & REMOVED VARBS w/ CORRELATION COEFFICIENTS >= 0.6
#3: Once ALIASED VARBS w/ HIGH VIFs were REMOVED: then REMOVED EVs w/ HIGH LINEAR CORRELATION VALUES that CONFOUNDED results from MULTICOLLINEAR ANALYSES - CORRELATION COEFFICIENT VALUES >= 0.6
#### Decision logic: remove based on highest VIF - if VIF similar (VIFDiff <= 2 - remove based on individual lower adjusted R^2)
# Declare model 2 landscape EVs - variables that are less than/equal to VIF cut off of 15
model2DaymetEVs <- c("AgBuf", "BurnRCA", "HiCascBuf", "HiCascRCA", "RoadsBuf", "RoadsRCA", "SLOPE", "WetlandBuf", "WetlandsRC", "h2oHiCascP", "h2oKm2", "h2oLakesPe")
# Subset model 2 landscape EVs from full correlation matrix
model2CorrMatrixDaymet <- fullCorrMatrixDaymet$r[model2DaymetEVs, model2DaymetEVs]
## Checking findCorrelation manually by looking at model2CorrMatrix
View(model2CorrMatrixDaymet)
# Find correlation coefficients >= 0.6
model3DaymetCorrResults <- findCorrelations(corrMatrix = model2CorrMatrixDaymet, corrCutOff = 0.6, inputEVs = model2DaymetEVs, VIFs = VIFsDaymet, data = SortedDaymetTSAndEVs2021)
model3DaymetVars <- model3DaymetCorrResults$finalEVs
print(model3DaymetVars) # Removed HiCascRCA, RoadsRCA, WetlandBuf, h2oKm2
## Keep: AgBuf, BurnRCA, HiCascBuf, RoadsBuf, SLOPE, WetlandsRC, h2oHiCascP, h2oLakesPe
# Declare model 3 landscape EVs & DAYMET - variables that have correlation coefficients > 0.6
model3DaymetEVs <- c("AgBuf", "BurnRCA", "HiCascBuf", "RoadsBuf", "SLOPE", "WetlandsRC", "h2oHiCascP", "h2oLakesPe")
# Subset model 3 landscape EVs from full correlation matrix
model3CorrMatrixDaymet <- fullCorrMatrixDaymet$r[model3DaymetEVs, model3DaymetEVs]
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs that have VIFS <= 15, correlation coefficients < 0.6 
model3Daymet <- lm(thermalSensitivity ~  AgBuf + BurnRCA + HiCascBuf + RoadsBuf + SLOPE + WetlandsRC + h2oHiCascP + h2oLakesPe, data = SortedDaymetTSAndEVs2021)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#4: Then used leaps R PACKAGE to select MULTIVARIATE REGRESSION that PRODUCED HIGHEST R^2
# Run regsubsets() on all model3 landscape variables
bestSubsetDaymet <-
    regsubsets(thermalSensitivity~., data =SortedDaymetTSAndEVs2021[ , c("thermalSensitivity", model3DaymetEVs)], nbest = 1, nvmax = length(model3DaymetEVs), force.in = NULL, force.out = NULL, method = "exhaustive")
summaryBestSubsetDaymet <- summary(bestSubsetDaymet)
as.data.frame(summaryBestSubsetDaymet$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(summaryBestSubsetDaymet$adjr2) # 4 recommended # of predictors
# What are the 4 best predictors
summaryBestSubsetDaymet$which[4, ]
# Keep AgBuf, BurnRCA, SLOPE, h2oHiCascP - old model had Shrub21 and h2oWetland
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs recommended by leaps 
model4Daymet <- lm(thermalSensitivity ~ AgBuf + BurnRCA + SLOPE + h2oHiCascP, data = SortedDaymetTSAndEVs2021)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#5: Then used lm.beta PACKAGE to EXTRACT STANDARDIZED BETA COEFFICIENT (estimates RELATIVE EFFECT of EACH EV on RV)
# Standardize using lm.beta
betaModel4Daymet <- lm.beta(model4Daymet)
# Creating model 4 info summary for comparison across steps - EVs with VIFs <= 15, correlation coefficients < 0.6, bestSubset predictors
model4DaymetInfoSummary <- createInfoSummary(model4Daymet, betaModel4Daymet, fullCorrMatrixDaymet, "model4Daymet")
write_csv(model4DaymetInfoSummary, "results/2021/MLR/daymet/Model 4 Daymet Info Summary.csv")

# Creates table format
xtable::xtable(betaModel4Daymet)

## Decreased adjusted R squared