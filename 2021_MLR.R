## Finalized MLR model using 2.4 Section as framework
# Choose VIF 15, correlation 0.6
# Landscape EVs: SLOPE, h2oLakesPe, h2oHiCascP, h2oWetland, Shrub21, BurnRCA (IMPORTANT)

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

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
# Define all EVs/landscape variables from Michael - remove Solar as an EV
EVs2021 <- c("SLOPE", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip")
# Loading RDS of thermal sensitivities joined w/ EV values for 2021
TSAndEVs2021 <- readRDS("results/2021/RDS/TSandEVs2021.RDS")
# Check
nrow(TSAndEVs2021) # No 1 AREMP site - 72 sites
# Load full correlation matrix
fullCorrMatrix <- readRDS("results/2021/correlation/RDS/fullCorrMatrix.RDS")
#------------------------------------------------------------------------------------------------------------------------------------------------
#1: BEFORE conducting MULTIVARIATE CORRELATIONS, TEST for MULTICOLLINEARITY among EVs by examining VIF factor & REMOVING any VALUES > 15
# Create linear regression w/ thermal sensitivity as RV & all landscape varbs as EVs
model1 <- lm(thermalSensitivity ~ SLOPE + Elev + BFI + h2oDevelop + h2oLakesPe + h2oAgricul + h2oBurnPer + h2oRdDens + h2oHiCascP + h2oWetland + h2oVegCov + h2oVegHt + Forest21 + Shrub21 + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + LakesRCA + HiCascRCA + DevelopRCA + RoadsRCA + VegCover + VegHeight_ + DevelopBuf + AgBuf + BurnBuf + WetlandBuf + LakesBuf + HiCascBuf + RoadsBuf + VegHtBuf + VegCovBuf + MeanMaxAir + MaxAir_C + Precip_mm + SumPrecip + MeanAirJJA + WetPrecip, data = TSAndEVs2021)
# Create lm beta model
betaModel1 <- lm.beta(model1)
# Creating model 1 info summary for comparison across steps - all EVs included
model1InfoSummary <- createInfoSummary(model1, betaModel1, fullCorrMatrix, "model1")
write_csv(model1InfoSummary, "results/2021/MLR/Model 1 Info Summary.csv")

# Calculate VIF values
VIFs <- vif(model1)
# View VIF values run on model1
print("All landscape variables with VIF values: ")
print(VIFs)
# Saving VIF values
write.csv(data.frame(Variable = names(VIFs), VIF = VIFs), "results/2021/MLR/model1VIFs.csv")

# Finding EVs greater than VIF cutoff 
EVsHighVIF <- VIFs[VIFs > 15]
print("Landscape variables with VIF > 15: ")
print(EVsHighVIF)
# Keep: SLOPE, h20LakesPe, h2oRdDens, h20HiCascP, Shrub21, h20km2, BurnRCA, WetlandsRC, HiCascRCA, RoadsRCA, AgBuf, WetlandBuf, HiCascBuf, RoadsBuf
# Take out EVsHighVIF from model1
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs that have VIFS <= 15
model2 <- lm(thermalSensitivity ~ SLOPE + h2oLakesPe + h2oRdDens + h2oHiCascP + Shrub21 + h2oKm2 + BurnRCA + WetlandsRC + HiCascRCA + RoadsRCA + AgBuf + WetlandBuf + HiCascBuf + RoadsBuf, data = TSAndEVs2021)
# Create lm beta model
betaModel2 <- lm.beta(model2)
# Creating model 2 info summary for comparison across steps - EVs with VIFs <= 15
model2InfoSummary <- createInfoSummary(model2, betaModel2, fullCorrMatrix, "model2")
write_csv(model2InfoSummary, "results/2021/MLR/Model 2 Info Summary.csv")
#-------------------------------------------------------------------------------------------------------------------------------------------------
#2: Case where VARBS are ALIASED: run PAIRWISE LINEAR CORRELATIONS on ALL EVs & REMOVED VARBS w/ CORRELATION COEFFICIENTS >= 0.6
#3: Once ALIASED VARBS w/ HIGH VIFs were REMOVED: then REMOVED EVs w/ HIGH LINEAR CORRELATION VALUES that CONFOUNDED results from MULTICOLLINEAR ANALYSES - CORRELATION COEFFICIENT VALUES >= 0.6
#### Decision logic: remove based on highest VIF - if VIF similar (VIFDiff <= 2 - remove based on individual lower adjusted R^2)
# Declare model 2 landscape EVs - variables that are less than/equal to VIF cut off of 15
model2EVs <- c("SLOPE", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "Shrub21", "h2oKm2", "BurnRCA", "WetlandsRC", "HiCascRCA", "RoadsRCA", "AgBuf", "WetlandBuf", "HiCascBuf", "RoadsBuf")
# Subset model 2 landscape EVs from full correlation matrix
model2CorrMatrix <- fullCorrMatrix$r[model2EVs, model2EVs]
## Checking findCorrelation manually by looking at model2CorrMatrix
View(model2CorrMatrix)
# SLOPE & h2okm2 = -0.7548 : removed h2oKm2
# h2oHiCascP & HiCascRCA = 0.7142 : removed HiCascRCA
# WetlandsRC & WetlandBuf = 0.8923 : removed WetlandsRC
# HiCascRCA & HiCascBuf = 0.8758 : SKIPPED since already removed HiCascRCA
# RoadsRCA & RoadsBuf = 0.6012 : removed RoadsRCA
# Find correlation coefficients >= 0.6
model3CorrResults <- findCorrelations(corrMatrix = model2CorrMatrix, corrCutOff = 0.6, inputEVs = model2EVs, VIFs = VIFs, data = TSAndEVs2021)
model3Vars <- model3CorrResults$finalEVs
print(model3Vars) # Removed h2oKm2, HiCascRCA, WetlandsRC, RoadsRCA
## Keep: SLOPE, h2oLakesPe, h2oRdDens, h2oHiCascP, Shrub21, BurnRCA, AgBuf, WetlandBuf, HiCascBuf, RoadsBuf
# Declare model 3 landscape EVs - variables that have correlation coefficients > 0.6
model3EVs <- c("SLOPE", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "Shrub21", "BurnRCA", "AgBuf", "WetlandBuf", "HiCascBuf", "RoadsBuf")
# Subset model 3 landscape EVs from full correlation matrix
model3CorrMatrix <- fullCorrMatrix$r[model3EVs, model3EVs]
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs that have VIFS <= 15, correlation coefficients < 0.6 
model3 <- lm(thermalSensitivity ~ SLOPE + h2oLakesPe + h2oRdDens + h2oHiCascP + Shrub21 + BurnRCA + AgBuf + WetlandBuf + HiCascBuf + RoadsBuf, data = TSAndEVs2021)
# Create lm beta model
betaModel3 <- lm.beta(model3)
# Creating model 3 info summary for comparison across steps - EVs with VIFs <= 15, correlation coefficients < 0.6
model3InfoSummary <- createInfoSummary(model3, betaModel3, fullCorrMatrix, "model3")
write_csv(model3InfoSummary, "results/2021/MLR/Model 3 Info Summary.csv")
#-------------------------------------------------------------------------------------------------------------------------------------------------
#4: Then used leaps R PACKAGE to select MULTIVARIATE REGRESSION that PRODUCED HIGHEST R^2
# Run regsubsets() on all model3 landscape variables
bestSubset <-
    regsubsets(thermalSensitivity~., data =TSAndEVs2021[ , c("thermalSensitivity", model3EVs)], nbest = 1, nvmax = length(model3EVs), force.in = NULL, force.out = NULL, method = "exhaustive")
summaryBestSubset <- summary(bestSubset)
as.data.frame(summaryBestSubset$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(summaryBestSubset$adjr2) # 5 recommended # of predictors
# What are the 5 best predictors
summaryBestSubset$which[5, ]
# Keep: SLOPE, h2oHiCascP, Shrub21, BurnRCA, AgBuf
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs recommended by leaps 
model4 <- lm(thermalSensitivity ~ SLOPE + h2oHiCascP + Shrub21 + BurnRCA + AgBuf, data = TSAndEVs2021)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#5: Then used lm.beta PACKAGE to EXTRACT STANDARDIZED BETA COEFFICIENT (estimates RELATIVE EFFECT of EACH EV on RV)
# Standardize using lm.beta
betaModel4 <- lm.beta(model4)
# Creating model 4 info summary for comparison across steps - EVs with VIFs <= 15, correlation coefficients < 0.6, bestSubset predictors
model4InfoSummary <- createInfoSummary(model4, betaModel4, fullCorrMatrix, "model4")
write_csv(model4InfoSummary, "results/2021/MLR/Model 4 Info Summary.csv")

# Creates table format
xtable::xtable(betaModel4)




# Checking model5
# Test model assumptions
# Residual plots
png("results/2021/MLR/model4ResidualPlots.png", width = 1200, height = 900)
par(mfrow = c(2,2))  # 2x2 grid
plot(betaModel4)
dev.off()
shapiro.test(residuals(betaModel4))  # Normality test
summary(betaModel4)$r.squared  # Check R²
summary(betaModel4)$adj.r.squared  # Check adjusted R²
