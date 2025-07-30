## Running MLR using 2.4 section - initial model

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()
#------------------------------------------------------------------------------------------------------------------------------------------------
# Join SortedTSAndEVs with DAYMET data & stream orientation 
# Load SortedDaymetTSAndEVs2021 from when DAYMET EVs were downloaded
SortedD_TS_EVs2021 <- readRDS("results/2021/RDS/SortedD_TS_EVs_2021.RDS")

# Read the 2021 orientation data - Index - Heading - Stream - LocalFlow - Azimuth - AbsAzimuth
orientation2021 <- read.csv("data/2021Orientation.csv")

# Join Sorted Daymet, TS, EVs, and Orientation data together by index - sorted N to S
library(tidyverse)
SortedO_D_TS_EVs2021 <- SortedD_TS_EVs2021 %>%
    left_join(orientation2021, by = c("index" = "Index"))

# Rename DAYMET EV columns to be more readable: old was daymet_dayl (s), daymet_prcp (mm/day), daymet_srad(W/m^2), daymet_swe(kg/m^2), daymet_tmax(deg c), daymet_tmin(deg c), daymet_vp (Pa)
SortedO_D_TS_EVs2021 <- SortedO_D_TS_EVs2021 %>%
    rename(daymetDayl = `daymet_dayl (s)`, daymetPrcp = `daymet_prcp (mm/day)`, daymetSRad = `daymet_srad (W/m^2)`, daymetSWE = `daymet_swe (kg/m^2)`, daymetTMax = `daymet_tmax (deg c)`, daymetTMin = `daymet_tmin (deg c)`, daymetVP = `daymet_vp (Pa)`)

# Selecting columns: take out Solar from Michael EVs, DAYMET SWE, tmax, tmin, & categorical varbs from Orientation
## Including index - site - x - y- thermalSensitivity - ... - 39 landscape EVs - daymetDayl, daymetPrcp, daymetSRad, daymetVP, Azimuth, AbsAzimuth
SortedO_D_TS_EVs2021 <- SortedO_D_TS_EVs2021 %>%
    select(-Solar, -daymetSWE, -daymetTMax, -daymetTMin, -Heading, -Stream, -Local.Flow)
saveRDS(SortedO_D_TS_EVs2021, "results/2021/RDS/SortedO_D_TS_EVs2021.RDS")
#------------------------------------------------------------------------------------------------------------------------------------------------
#1: BEFORE conducting MULTIVARIATE CORRELATIONS, 
# TEST for MULTICOLLINEARITY among EVs by examining VIF factor
# & REMOVING any VALUES > 5

# Loading RDS of thermal sensitivities joined w/ EV values for 2021
TSAndEVs2021 <- readRDS("results/2021/RDS/TSandEVs2021.RDS")
# Check
nrow(TSAndEVs2021)

# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs
a00.model1 <- lm(thermalSensitivity ~ SLOPE + Solar + Elev + BFI + h2oDevelop + h2oLakesPe + h2oAgricul + h2oBurnPer + h2oRdDens + h2oHiCascP + h2oWetland + h2oVegCov + h2oVegHt + Forest21 + Shrub21 + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + LakesRCA + HiCascRCA + DevelopRCA + RoadsRCA + VegCover + VegHeight_ + DevelopBuf + AgBuf + BurnBuf + WetlandBuf + LakesBuf + HiCascBuf + RoadsBuf + VegHtBuf + VegCovBuf + MeanMaxAir + MaxAir_C + Precip_mm + SumPrecip + MeanAirJJA + WetPrecip, data = TSAndEVs2021)

# Load library to calculate VIF values
# install.packages("car")
library(car)
# Calculate VIF values
a00.VIFs <- vif(a00.model1)
# View VIF values run on model1
print("All landscape variables with VIF values: ")
print(a00.VIFs)
# Saving VIF values
write.csv(data.frame(Variable = names(a00.VIFs), VIF = a00.VIFs), "results/2021/MLR/initialModel/a00.VIFs.csv")

# If use VIF > 5: only SLOPE would be included... 
# If use VIF > 10: only 6 landscape variables
# If use VIF > 20: 14 landscape variables
a00.EVsHighVIF <- a00.VIFs[a00.VIFs > 20]
print("Landscape variables with VIF > 20: ")
print(a00.EVsHighVIF)

# Take out EVsHighVIF from model1
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs that have VIFS <= 20
a00.model2 <- lm(thermalSensitivity ~ SLOPE + Solar + h2oLakesPe + h2oRdDens + h2oHiCascP + h2oWetland + Shrub21 + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + HiCascRCA + RoadsRCA + AgBuf + BurnBuf + WetlandBuf + HiCascBuf + RoadsBuf, data = TSAndEVs2021)


#-------------------------------------------------------------------------------------------------------------------------------------------------
#2: Case where VARBS are ALIASED: run PAIRWISE LINEAR CORRELATIONS on ALL EVs 
# & REMOVED VARBS w/ CORRELATION COEFFICIENTS > 0.9

# Load full correlation matrix 
fullCorrMatrix <- readRDS("results/2021/correlation/RDS/fullCorrMatrix.RDS")
a00.model2Vars <- c("SLOPE", "Solar", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "h2oWetland", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "HiCascRCA", "RoadsRCA", "AgBuf", "BurnBuf", "WetlandBuf", "HiCascBuf", "RoadsBuf")

# Subset model2Vars from full correlation matrix
a00.model2CorrMatrix <- fullCorrMatrix$r[a00.model2Vars, a00.model2Vars]
# Look for any correlation coefficients > 0.9 (ignore self-correlations OBVIII)
View(a00.model2CorrMatrix)
write.csv(a00.model2CorrMatrix, "results/2021/MLR/initialModel/a00.model2CorrMatrix.csv")
# No correlation coefficients > 0.9

#-------------------------------------------------------------------------------------------------------------------------------------------------
#3: Once ALIASED VARBS w/ HIGH VIFs were REMOVED: 
# then REMOVED EVs w/ HIGH LINEAR CORRELATION VALUES that 
# CONFOUNDED results from MULTICOLLINEAR ANALYSES - CORRELATION COEFFICIENT VALUES >= 0.6

# Now find EVs with correlation coefficents >= 0.6: 
# SLOPE & h2oKm2 : r = -0.7548
## Stream channel slope & upstream area km2
### VIFs: 4.038493 vs. 12.723293 - REMOVE h20km2
summary(lm(thermalSensitivity ~ SLOPE, data = TSAndEVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ h2oKm2, data = TSAndEVs2021))$adj.r.squared
### adj R^2: 0.02 vs. 0a00.7 - REMOVE h20km2 both not good though
summary(lm(thermalSensitivity ~ SLOPE, data = TSAndEVs2021))
summary(lm(thermalSensitivity ~ h2oKm2, data = TSAndEVs2021))
# DROP: h20km2 (SLOPE lower VIF, smaller std error)


# h2oHiCascP & HiCascRCA : r = 0.7142
## Upstream high cascades geology & catchment high cascades area %
### VIFs: 10.047336 vs. 9.448280 - REMOVE h20HiCascP but SIMILAR
summary(lm(thermalSensitivity ~ h2oHiCascP, data = TSAndEVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs2021))$adj.r.squared
### adj R^2: 0.56 vs. 0.19 - REMOVE HiCascRCA 
summary(lm(thermalSensitivity ~ h2oHiCascP, data = TSAndEVs2021))
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs2021))
# DROP: HiCascRCA (h2oHiCascP much larger adj R62, more significant)


# AgricultRC & AgBuf : r = 0.8356 
## Catchment agricultural area % & buffer agricultural area %
### VIFs: 18.036515 vs. 11.209889 - REMOVE AgricultRC
summary(lm(thermalSensitivity ~ AgricultRC, data = TSAndEVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ AgBuf, data = TSAndEVs2021))$adj.r.squared
### adj R^2: 0.26 vs. 0.08 - REMOVE AgBuf 
summary(lm(thermalSensitivity ~ AgricultRC, data = TSAndEVs2021))
summary(lm(thermalSensitivity ~ AgBuf, data = TSAndEVs2021))
# DROP: AgBuf (AgricultRC has higher VIF BUT, higher adj R^2, lower std error, stronger sig)


# BurnBuf & BurnRCA : r = 0.819
## Buffer burn area % & catchment burn area %
### VIFs: 17.661771 vs. 8.875225 - REMOVE BurnBuf
summary(lm(thermalSensitivity ~ BurnBuf, data = TSAndEVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ BurnRCA, data = TSAndEVs2021))$adj.r.squared
### adj R^2: -0a00.1 vs. -0.010 - REMOVE BurnRCA but both bad
summary(lm(thermalSensitivity ~ BurnBuf, data = TSAndEVs2021))
summary(lm(thermalSensitivity ~ BurnRCA, data = TSAndEVs2021))
# DROP: BurnBuf (BurnRCA has lower VIF but both LOW adj R^2)


# WetlandsRC & WetlandBuf : r = 0.8923
## catchment wetlands area % & buffer wetland area %
### VIFs: 9.208864 vs. 6.790878 - REMOVE WetlandsRC
summary(lm(thermalSensitivity ~ WetlandsRC, data = TSAndEVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ WetlandBuf, data = TSAndEVs2021))$adj.r.squared
### adj R^2: 0.02 vs. 0a00.4 - REMOVE WetlandBuf
summary(lm(thermalSensitivity ~ WetlandsRC, data = TSAndEVs2021))
summary(lm(thermalSensitivity ~ WetlandBuf, data = TSAndEVs2021))
# DROP: WetlandBuf (WetlandsRC higher adj R^2, lower p-value despite higher VIF)


# HiCascRCA & HiCascBuf : r = 0.8758 
## Catchment high casc area % & buffer high casc area %
### VIFs: 9.448280 vs. 5.630197 - REMOVE HiCascRCA
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ HiCascBuf, data = TSAndEVs2021))$adj.r.squared
### adj R^2: 0.19 vs. 0.12 - REMOVE HiCascBuf but SIMILAR
summary(lm(thermalSensitivity ~ HiCascRCA, data = TSAndEVs2021))
summary(lm(thermalSensitivity ~ HiCascBuf, data = TSAndEVs2021))
# DROP: HiCascRCA since ALREADY DROPPED - keep HiCascBuf


# RoadsRCA & RoadsBuf : r = 0.6012
## catchment road density & buffer road density 
### VIFs: 10.457899 vs. 6.504549 0 REMOVE RoadsRCA
summary(lm(thermalSensitivity ~ RoadsRCA, data = TSAndEVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ RoadsBuf, data = TSAndEVs2021))$adj.r.squared
### adj R^2: -0.01 vs. -0.01 - BOTH BAD
summary(lm(thermalSensitivity ~ RoadsRCA, data = TSAndEVs2021))
summary(lm(thermalSensitivity ~ RoadsBuf, data = TSAndEVs2021))
# DROP: RoadsRCA (RoadsBuf had lower VIF but could DROP BOTH since adj R^2 bad)

# Landscape variables being dropped: h20km2, HiCascRCA, AgBuf, BurnBuf, WetlandBuf, RoadsRCA

# Take out dropped landscape variables from model2
# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs that confound results
a00.model3 <- lm(thermalSensitivity ~ SLOPE + Solar + h2oLakesPe + h2oRdDens + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA + AgricultRC + WetlandsRC + HiCascBuf + RoadsBuf, data = TSAndEVs2021)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#4: Then used leaps R PACKAGE to select MULTIVARIATE REGRESSION that PRODUCED HIGHEST R^2
# Load leaps package
# install.packages("leaps")
library(leaps)

# Define landscape variables from model3
a00.model3Vars <- c("SLOPE", "Solar", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "h2oWetland", "Shrub21", "BurnRCA", "AgricultRC", "WetlandsRC", "HiCascBuf", "RoadsBuf")

# Run regsubsets() on all model3 landscape variables
a00.bestSubset <-
    regsubsets(thermalSensitivity~., data =TSAndEVs2021[ , c("thermalSensitivity", a00.model3Vars)], nbest = 1, nvmax = length(a00.model3Vars), force.in = NULL, force.out = NULL, method = "exhaustive")
a00.summaryBestSubset <- summary(a00.bestSubset)
as.data.frame(a00.summaryBestSubset$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(a00.summaryBestSubset$adjr2) # 9 
# What are the best predictors
a00.summaryBestSubset$which[9, ]
# SLOPE, Solar, h2oLakesPe, h2oHiCascP, h2oWetland, Shrub21, BurnRCA, AgricultRC, WetlandsRC

# Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs recommended by leaps 
a00.model4 <- lm(thermalSensitivity ~ SLOPE + Solar + h2oLakesPe + h2oHiCascP + h2oWetland + Shrub21 + BurnRCA + AgricultRC + WetlandsRC, data = TSAndEVs2021)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#5: Then used lm.beta PACKAGE to EXTRACT STANDARDIZED BETA COEFFICIENT 
# (estimates RELATIVE EFFECT of EACH EV on RV)

# Load package
# install.packages("lm.beta")
library(lm.beta)

# Standardize using lm.beta
a00.model5 <- lm.beta(a00.model4)
print(a00.model5)
# Summary with standarized coefficients
summary(a00.model5)
# Extracts standardized beta coefficients only
coef(a00.model5)
# Creates table format
xtable::xtable(a00.model5)

# Saving coefficient table from model5
# Create data frame
a00.model5Coeff <- as.data.frame(summary(a00.model5)$coefficients)
# Save to CSV
write.csv(a00.model5Coeff, "results/2021/MLR/initialModel/a00.model5Coeff.csv", row.names = TRUE)



# Checking model5
# Test model assumptions
# Residual plots
png("results/2021/MLR/initialModel/a00.model4ResidualPlots.png", width = 1200, height = 900)
par(mfrow = c(2,2))  # 2x2 grid
plot(a00.model5)
dev.off()
shapiro.test(residuals(a00.model5))  # Normality test
summary(a00.model5)$r.squared  # Check R²
summary(a00.model5)$adj.r.squared  # Check adjusted R²

#----------------------------------------------------------------------------------------------------------------
## Testing individual models

# Change landscape EVs
baseModelVars <- c("SLOPE", "Solar", "h2oLakesPe", "h2oHiCascP", "h2oWetland", "Shrub21", "h2oKm2", "BurnRCA")
# change 2x : vector & nvmax
baseBestSubset <-
    regsubsets(thermalSensitivity~., data =TSAndEVs2021[ , c("thermalSensitivity", baseModelVars)], nbest = 1, nvmax = length(baseModelVars), force.in = NULL, force.out = NULL, method = "exhaustive")
baseSummaryBestSubset <- summary(baseBestSubset)
as.data.frame(baseSummaryBestSubset$outmat)
which.max(baseSummaryBestSubset$adjr2)
baseSummaryBestSubset$which[8, ] # Change num - copy paste below but don't forget data = TSAndEVs2021
baseModel <- lm(thermalSensitivity~ SLOPE+Solar+h2oLakesPe+h2oHiCascP+h2oWetland+Shrub21+h2oKm2+BurnRCA, data=TSAndEVs2021)
# Summary
baseModelStd <- lm.beta(baseModel)
summary(baseModelStd)
coef(baseModelStd)
summary(baseModelStd)$adj.r.squared 
