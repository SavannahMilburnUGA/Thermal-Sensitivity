# Running MLR using 2.4 section

#-------------------------------------------------------------------------------------------------------------------------------------------------
#1: BEFORE conducting MULTIVARIATE CORRELATIONS, 
# TEST for MULTICOLLINEARITY among EVs by examining VIF factor
# & REMOVING any VALUES > 5

# Loading RDS of thermal sensitivities joined w/ EV values for 2021
TSAndEVs <- readRDS("TSandEVs.RDS")
# Check
nrow(TSAndEVs)
# Saving massive spreadsheet
write.csv(TSAndEVs, "TSAndEVs.csv")

# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs
model1 <- lm(thermalSensitivity ~ SLOPE + Solar + Elev + BFI + h2oDevelop + h2oLakesPe + h2oAgricul + h2oBurnPer + h2oRdDens + h2oHiCascP + h2oWetland + h2oVegCov + h2oVegHt + Forest21 + Shrub21 + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + LakesRCA + HiCascRCA + DevelopRCA + RoadsRCA + VegCover + VegHeight_ + DevelopBuf + AgBuf + BurnBuf + WetlandBuf + LakesBuf + HiCascBuf + RoadsBuf + VegHtBuf + VegCovBuf + MeanMaxAir + MaxAir_C + Precip_mm + SumPrecip + MeanAirJJA + WetPrecip, data = TSAndEVs)

# Load library to calculate VIF values
# install.packages("car")
library(car)
# Calculate VIF values
VIFs <- vif(model1)
# View VIF values run on model1
print("All landscape variables with VIF values: ")
print(VIFs)
# Saving VIF values
write.csv(data.frame(Variable = names(VIFs), VIF = VIFs), "VIFs.csv")

# If use VIF > 5: only SLOPE would be included... 
# If use VIF > 10: only 6 landscape variables
# If use VIF > 20: 14 landscape variables
EVsHighVIF <- VIFs[VIFs > 20]
print("Landscape variables with VIF > 20: ")
print(EVsHighVIF)

# Take out EVsHighVIF from model1
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs that have VIFS <= 20
model2 <- lm(thermalSensitivity ~ SLOPE + Solar + h2oLakesPe + h2oRdDens + h2oHiCascP + h2oWetland + Shrub21 + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + HiCascRCA + RoadsRCA + AgBuf + BurnBuf + WetlandBuf + HiCascBuf + RoadsBuf, data = TSAndEVs)


#-------------------------------------------------------------------------------------------------------------------------------------------------
#2: Case where VARBS are ALIASED: run PAIRWISE LINEAR CORRELATIONS on ALL EVs 
# & REMOVED VARBS w/ CORRELATION COEFFICIENTS > 0.9

# Load full correlation matrix 
fullCorrMatrix <- readRDS("fullCorrelationMatrix.RDS")
model2Vars <- c("SLOPE", "Solar", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "h2oWetland", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "HiCascRCA", "RoadsRCA", "AgBuf", "BurnBuf", "WetlandBuf", "HiCascBuf", "RoadsBuf")

# Subset model2Vars from full correlation matrix
model2CorrMatrix <- fullCorrMatrix[model2Vars, model2Vars]
# Look for any correlation coefficients > 0.9 (ignore self-correlations OBVIII)
View(model2CorrMatrix)
write.csv(model2CorrMatrix, "model2CorrMatrix.csv")
# No correlation coefficients > 0.9

#-------------------------------------------------------------------------------------------------------------------------------------------------
#3: Once ALIASED VARBS w/ HIGH VIFs were REMOVED: 
# then REMOVED EVs w/ HIGH LINEAR CORRELATION VALUES that 
# CONFOUNDED results from MULTICOLLINEAR ANALYSES - CORRELATION COEFFICIENT VALUES >= 0.6

# Now find EVs with correlation coefficents >= 0.6: 
# SLOPE & h2oKm2 : r = -0.7548
## Stream channel slope & upstream area km2
### VIFs: 4.038493 vs. 12.723293 - REMOVE h20km2
summary(lm(thermalSensitivity ~ SLOPE, data = TSAndEVs))$adj.r.squared
summary(lm(thermalSensitivity ~ h2oKm2, data = TSAndEVs))$adj.r.squared
### adj R^2: 0.02 vs. 0.007 - REMOVE h20km2 both not good though
summary(lm(thermalSensitivity ~ SLOPE, data = TSAndEVs))
summary(lm(thermalSensitivity ~ h2oKm2, data = TSAndEVs))
# DROP: h20km2 (SLOPE lower VIF, smaller std error)


# h2oHiCascP & HiCascRCA : r = 0.7142
## Upstream high cascades geology & catchment high cascades area %
### VIFs: 10.047336 vs. 9.448280 - REMOVE h20HiCascP but SIMILAR
summary(lm(thermalSensitivity ~ h2oHiCascP, data = TSAndEVs))$adj.r.squared
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs))$adj.r.squared
### adj R^2: 0.56 vs. 0.19 - REMOVE HiCascRCA 
summary(lm(thermalSensitivity ~ h2oHiCascP, data = TSAndEVs))
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs))
# DROP: HiCascRCA (h2oHiCascP much larger adj R62, more significant)


# AgricultRC & AgBuf : r = 0.8356 
## Catchment agricultural area % & buffer agricultural area %
### VIFs: 18.036515 vs. 11.209889 - REMOVE AgricultRC
summary(lm(thermalSensitivity ~ AgricultRC, data = TSAndEVs))$adj.r.squared
summary(lm(thermalSensitivity ~ AgBuf, data = TSAndEVs))$adj.r.squared
### adj R^2: 0.26 vs. 0.08 - REMOVE AgBuf 
summary(lm(thermalSensitivity ~ AgricultRC, data = TSAndEVs))
summary(lm(thermalSensitivity ~ AgBuf, data = TSAndEVs))
# DROP: AgBuf (AgricultRC has higher VIF BUT, higher adj R^2, lower std error, stronger sig)


# BurnBuf & BurnRCA : r = 0.819
## Buffer burn area % & catchment burn area %
### VIFs: 17.661771 vs. 8.875225 - REMOVE BurnBuf
summary(lm(thermalSensitivity ~ BurnBuf, data = TSAndEVs))$adj.r.squared
summary(lm(thermalSensitivity ~ BurnRCA, data = TSAndEVs))$adj.r.squared
### adj R^2: -0.001 vs. -0.010 - REMOVE BurnRCA but both bad
summary(lm(thermalSensitivity ~ BurnBuf, data = TSAndEVs))
summary(lm(thermalSensitivity ~ BurnRCA, data = TSAndEVs))
# DROP: BurnBuf (BurnRCA has lower VIF but both LOW adj R^2)


# WetlandsRC & WetlandBuf : r = 0.8923
## catchment wetlands area % & buffer wetland area %
### VIFs: 9.208864 vs. 6.790878 - REMOVE WetlandsRC
summary(lm(thermalSensitivity ~ WetlandsRC, data = TSAndEVs))$adj.r.squared
summary(lm(thermalSensitivity ~ WetlandBuf, data = TSAndEVs))$adj.r.squared
### adj R^2: 0.02 vs. 0.004 - REMOVE WetlandBuf
summary(lm(thermalSensitivity ~ WetlandsRC, data = TSAndEVs))
summary(lm(thermalSensitivity ~ WetlandBuf, data = TSAndEVs))
# DROP: WetlandBuf (WetlandsRC higher adj R^2, lower p-value despite higher VIF)


# HiCascRCA & HiCascBuf : r = 0.8758 
## Catchment high casc area % & buffer high casc area %
### VIFs: 9.448280 vs. 5.630197 - REMOVE HiCascRCA
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs))$adj.r.squared
summary(lm(thermalSensitivity ~ HiCascBuf, data = TSAndEVs))$adj.r.squared
### adj R^2: 0.19 vs. 0.12 - REMOVE HiCascBuf but SIMILAR
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs))
summary(lm(thermalSensitivity ~ HiCascBuf, data = TSAndEVs))
# DROP: HiCascRCA since ALREADY DROPPED - keep HiCascBuf


# RoadsRCA & RoadsBuf : r = 0.6012
## catchment road density & buffer road density 
### VIFs: 10.457899 vs. 6.504549 0 REMOVE RoadsRCA
summary(lm(thermalSensitivity ~ RoadsRCA, data = TSAndEVs))$adj.r.squared
summary(lm(thermalSensitivity ~ RoadsBuf, data = TSAndEVs))$adj.r.squared
### adj R^2: -0.01 vs. -0.01 - BOTH BAD
summary(lm(thermalSensitivity ~ RoadsRCA, data = TSAndEVs))
summary(lm(thermalSensitivity ~ RoadsBuf, data = TSAndEVs))
# DROP: RoadsRCA (RoadsBuf had lower VIF but could DROP BOTH since adj R^2 bad)

# Landscape variables being dropped: h20km2, HiCascRCA, AgBuf, BurnBuf, WetlandBuf, RoadsRCA

# Take out dropped landscape variables from model2
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs that confound results
model3 <- lm(thermalSensitivity ~ SLOPE + Solar + h2oLakesPe + h2oRdDens + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA + AgricultRC + WetlandsRC + HiCascBuf + RoadsBuf, data = TSAndEVs)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#4: Then used leaps R PACKAGE to select MULTIVARIATE REGRESSION that PRODUCED HIGHEST R^2
# Load leaps package
# install.packages("leaps")
library(leaps)

# Define landscape variables from model3
model3Vars <- c("SLOPE", "Solar", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "h2oWetland", "Shrub21", "BurnRCA", "AgricultRC", "WetlandsRC", "HiCascBuf", "RoadsBuf")

# Run regsubsets() on all model3 landscape variables
bestSubset <-
    regsubsets(thermalSensitivity~., data =TSAndEVs[ , c("thermalSensitivity", model3Vars)], nbest = 1, nvmax = length(model3Vars), force.in = NULL, force.out = NULL, method = "exhaustive")
summaryBestSubset <- summary(bestSubset)
as.data.frame(summaryBestSubset$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(summaryBestSubset$adjr2) # 9 
# What are the best predictors
summaryBestSubset$which[9, ]
# SLOPE, Solar, h2oLakesPe, h2oHiCascP, h2oWetland, Shrub21, BurnRCA, AgricultRC, WetlandsRC

# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs recommended by leaps 
model4 <- lm(thermalSensitivity ~ SLOPE + Solar + h2oLakesPe + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA + AgricultRC + WetlandsRC, data = TSAndEVs)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#5: Then used lm.beta PACKAGE to EXTRACT STANDARDIZED BETA COEFFICIENT 
# (estimates RELATIVE EFFECT of EACH EV on RV)

# Load package
# install.packages("lm.beta")
library(lm.beta)

# Standardize using lm.beta
model5 <- lm.beta(model4)
print(model5)
# Summary with standarized coefficients
summary(model5)
# Extracts standardized beta coefficients only
coef(model5)
# Creates table format
xtable::xtable(model5)

# Saving coefficient table from model5
# Create data frame
model5Coeff <- as.data.frame(summary(model5)$coefficients)
# Save to CSV
write.csv(model5Coeff, "model5Coeff.csv", row.names = TRUE)



# Checking model5
# Test model assumptions
# Residual plots
png("model4ResidualPlots.png", width = 1200, height = 900)
par(mfrow = c(2,2))  # 2x2 grid
plot(model5)
dev.off()
shapiro.test(residuals(model5))  # Normality test
summary(model5)$r.squared  # Check R²
summary(model5)$adj.r.squared  # Check adjusted R²

