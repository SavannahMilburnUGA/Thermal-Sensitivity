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

# Create linear regression w/ thermal sensitivity as RV & 39 landscape + 4 DAYMET + 2 Orientation as EVs
a00.model1 <- lm(thermalSensitivity ~ SLOPE + Elev + BFI + h2oDevelop + h2oLakesPe + h2oAgricul + h2oBurnPer + h2oRdDens + h2oHiCascP + h2oWetland + h2oVegCov + h2oVegHt + Forest21 + Shrub21 + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + LakesRCA + HiCascRCA + DevelopRCA + RoadsRCA + VegCover + VegHeight_ + DevelopBuf + AgBuf + BurnBuf + WetlandBuf + LakesBuf + HiCascBuf + RoadsBuf + VegHtBuf + VegCovBuf + MeanMaxAir + MaxAir_C + Precip_mm + SumPrecip + MeanAirJJA + WetPrecip + daymetDayl + daymetPrcp + daymetSRad + daymetVP + Azimuth + AbsAzimuth, data = SortedO_D_TS_EVs2021)

# Load library to calculate VIF values
# install.packages("car")
library(car)
# Calculate VIF values
a00.VIFs <- vif(a00.model1)
# View VIF values run on model1
print("39 landscape + 4 DAYMET + 2 Orientation EVs with VIF values: ")
print(a00.VIFs) # Note: new DAYMET EVs are ~118, 51, 41, 239... BUT Orientations are SMALL
# Saving VIF values
write.csv(data.frame(Variable = names(a00.VIFs), VIF = a00.VIFs), "results/2021/MLR/initialModel/a00.VIFs.csv")

# Using VIF > 20
a00.EVsHighVIF <- a00.VIFs[a00.VIFs > 20]
print("39 landscape + 4 DAYMET + 2 Orientation EVs with VIF > 20: ")
print(a00.EVsHighVIF)
# Removing 28 EVs: Elev, BFI, h2oDevelop, h2oAgricul, h2oBurnPer, h2oWetland, h2oVegCov, h2oVegHt, Forest21, Shrub21, LakesRCA, DevelopRCA, VegCover, VegHeight_
# DevelopBuf, LakesBuf, VegHtBuf, VegCovBuf, MeanMaxAir, MaxAir_C, Precip_mm, SumPrecip, MeanAirJJA, WetPrecip, daymetDayl, daymetPrcp, daymetSRad, daymetVP 
# Take out EVsHighVIF from model1
# Create linear regression w/ thermal sensitivity as RV & 39 landscape + 4 DAYMET + 2 Orientation as EVs that have VIFS <= 20
# Only have 17 EVs now: 
a00.model2 <- lm(thermalSensitivity ~ SLOPE + h2oLakesPe + h2oRdDens + h2oHiCascP + h2oKm2 + BurnRCA + AgricultRC + WetlandsRC + HiCascRCA + RoadsRCA + AgBuf + BurnBuf + WetlandBuf + HiCascBuf + RoadsBuf + Azimuth + AbsAzimuth, data = SortedO_D_TS_EVs2021)


#-------------------------------------------------------------------------------------------------------------------------------------------------
#2: Case where VARBS are ALIASED: run PAIRWISE LINEAR CORRELATIONS on ALL EVs 
# & REMOVED VARBS w/ CORRELATION COEFFICIENTS > 0.9

# Load full correlation matrix 
full39_ODT_CorrMatrix <- readRDS("results/2021/correlation/RDS/full39_ODT_CorrMatrix.RDS")
a00.model2Vars <- c("SLOPE", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "HiCascRCA", "RoadsRCA", "AgBuf", "BurnBuf", "WetlandBuf", "HiCascBuf", "RoadsBuf", "Azimuth", "AbsAzimuth")

# Subset model2Vars from full correlation matrix
a00.model2ODT_CorrMatrix <- full39_ODT_CorrMatrix$r[a00.model2Vars, a00.model2Vars]
# Look for any correlation coefficients > 0.9 (ignore self-correlations OBVIII)
View(a00.model2ODT_CorrMatrix)
write.csv(a00.model2ODT_CorrMatrix, "results/2021/MLR/initialModel/a00.model2ODT_CorrMatrix.csv")
# No correlation coefficients > 0.9

#-------------------------------------------------------------------------------------------------------------------------------------------------
#3: Once ALIASED VARBS w/ HIGH VIFs were REMOVED: 
# then REMOVED EVs w/ HIGH LINEAR CORRELATION VALUES that 
# CONFOUNDED results from MULTICOLLINEAR ANALYSES - CORRELATION COEFFICIENT VALUES >= 0.6

# SLOPE & h2oKm2 : r = -0.7548
### VIFs: 7.1, 16.2 REMOVE h2oKm2
summary(lm(thermalSensitivity ~ SLOPE, data = SortedO_D_TS_EVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ h2oKm2, data = SortedO_D_TS_EVs2021))$adj.r.squared
# DROP: h2oKm2

# h2oHiCascP & HiCascRCA : r = 0.7142
### VIFs: 14.7, 11.1 REMOVE h2oHiCascP
summary(lm(thermalSensitivity ~ h2oHiCascP, data = SortedO_D_TS_EVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ HiCascRCA, data = SortedO_D_TS_EVs2021))$adj.r.squared
# DROP: HiCascRCA

# BurnRCA & BurnBuf :  r = 0.819
### VIFs: 12.3, 19.8 REMOVE BurnBuf
summary(lm(thermalSensitivity ~ BurnRCA, data = SortedO_D_TS_EVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ BurnBuf, data = SortedO_D_TS_EVs2021))$adj.r.squared
# DROP: BurnBuf BUT BOTH BAD

# AgricultRC & AgBuf :  r = 0.8356
### VIFs: 17.4, 15.7 TOO SIMILAR
summary(lm(thermalSensitivity ~ AgricultRC, data = SortedO_D_TS_EVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ AgBuf, data = SortedO_D_TS_EVs2021))$adj.r.squared
# DROP: AgBuf


# WetlandsRC & WetlandBuf :  r = 0.8923
### VIFs: 10.2, 8.5 TOO SIMILAR
summary(lm(thermalSensitivity ~ WetlandsRC, data = SortedO_D_TS_EVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ WetlandBuf, data = SortedO_D_TS_EVs2021))$adj.r.squared
# DROP: WetlandBuf

# HiCascRCA & HiCascBuf :  r = 0.8758
### VIFs: 11.1, 5.7 REMOVE HiCascRCA
summary(lm(thermalSensitivity ~ HiCascRCA, data = SortedO_D_TS_EVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ HiCascBuf, data = SortedO_D_TS_EVs2021))$adj.r.squared
# DROP: HiCascRCA

# RoadsRCA & RoadsBuf :  r = 0.6012
### VIFs: 11.0, 7.0 REMOVE RoadsRCA
summary(lm(thermalSensitivity ~ RoadsRCA, data = SortedO_D_TS_EVs2021))$adj.r.squared
summary(lm(thermalSensitivity ~ RoadsBuf, data = SortedO_D_TS_EVs2021))$adj.r.squared
# DROP: RoadsRCA but BOTH BAD


# Landscape variables being dropped: h2oKm2, HiCascRCA, BurnBuf, AgBuf, WetlandBuf, RoadsRCA - !!!! BUTTTT BurnBuf & RoadsRCA both bad

# Take out dropped landscape variables from model2
# Create linear regression w/ thermal sensitivity as RV & EVs that confound results
# Now have 11 (NOTE: BurnBuf, RoadsRCA both bad also)
a00.model3 <- lm(thermalSensitivity ~ SLOPE + h2oLakesPe + h2oRdDens + h2oHiCascP + BurnRCA + AgricultRC + WetlandsRC + HiCascBuf + RoadsBuf + Azimuth + AbsAzimuth, data = SortedO_D_TS_EVs2021)
#-------------------------------------------------------------------------------------------------------------------------------------------------
#4: Then used leaps R PACKAGE to select MULTIVARIATE REGRESSION that PRODUCED HIGHEST R^2
# Load leaps package
# install.packages("leaps")
library(leaps)

# Define landscape variables from model3
a00.model3Vars <- c("SLOPE", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "BurnRCA", "AgricultRC", "WetlandsRC", "HiCascBuf", "RoadsBuf", "Azimuth", "AbsAzimuth")

# Run regsubsets() on all model3 landscape variables
a00.bestSubset <-
    regsubsets(thermalSensitivity~., data =SortedO_D_TS_EVs2021[ , c("thermalSensitivity", a00.model3Vars)], nbest = 1, nvmax = length(a00.model3Vars), force.in = NULL, force.out = NULL, method = "exhaustive")
a00.summaryBestSubset <- summary(a00.bestSubset)
as.data.frame(a00.summaryBestSubset$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(a00.summaryBestSubset$adjr2) # 4
# What are the best predictors
a00.summaryBestSubset$which[4, ]
# SLOPE, h2oRdDens, h2oHiCascP, AgricultRC

# Create linear regression w/ thermal sensitivity as RV & EVs recommended by leaps 
a00.model4 <- lm(thermalSensitivity ~ SLOPE + h2oRdDens + h2oHiCascP + AgricultRC, data = SortedO_D_TS_EVs2021)
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
summary(a00.model5) # 3/4 SIG w/ adj-R-squared 0.6363
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

#-------------------------------------------------------------------------------------------------------------------------------------------------
# (NOTE: BurnRCA, RoadsBuf both bad also) - taking out cuz they individually did not explain that much variability in TS

# MODEL A)
# Removing RoadsBuf
a00.model3A <- lm(thermalSensitivity ~ SLOPE + h2oLakesPe + h2oRdDens + h2oHiCascP + BurnRCA + AgricultRC + WetlandsRC + HiCascBuf + Azimuth + AbsAzimuth, data = SortedO_D_TS_EVs2021)
# Define landscape variables from model3
a00.model3VarsA <- c("SLOPE", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "BurnRCA", "AgricultRC", "WetlandsRC", "HiCascBuf", "Azimuth", "AbsAzimuth")

# Run regsubsets() on all model3 landscape variables
a00.bestSubsetA <-
    regsubsets(thermalSensitivity~., data =SortedO_D_TS_EVs2021[ , c("thermalSensitivity", a00.model3VarsA)], nbest = 1, nvmax = length(a00.model3VarsA), force.in = NULL, force.out = NULL, method = "exhaustive")
a00.summaryBestSubsetA <- summary(a00.bestSubsetA)
as.data.frame(a00.summaryBestSubsetA$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(a00.summaryBestSubsetA$adjr2) # 4
# What are the best predictors
a00.summaryBestSubsetA$which[4, ]
# SLOPE, h2oRdDens, h2oHiCAscp, AgricultRC
# Create linear regression w/ thermal sensitivity as RV & EVs recommended by leaps 
a00.model4A <- lm(thermalSensitivity ~ SLOPE + h2oRdDens + h2oHiCascP + AgricultRC, data = SortedO_D_TS_EVs2021)
a00.model5A <- lm.beta(a00.model4A)
# Summary with standarized coefficients
summary(a00.model5A) # 3/4 SIG w/ adj-R-squared 0.6363 - same model
#--------------------------------------------------------
# MODEL B)
# Removing BurnRCA
a00.model3B <- lm(thermalSensitivity ~ SLOPE + h2oLakesPe + h2oRdDens + h2oHiCascP + AgricultRC + WetlandsRC + HiCascBuf + RoadsBuf + Azimuth + AbsAzimuth, data = SortedO_D_TS_EVs2021)
# Define landscape variables from model3
a00.model3VarsB <- c("SLOPE", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "AgricultRC", "WetlandsRC", "HiCascBuf", "RoadsBuf", "Azimuth", "AbsAzimuth")

# Run regsubsets() on all model3 landscape variables
a00.bestSubsetB <-
    regsubsets(thermalSensitivity~., data =SortedO_D_TS_EVs2021[ , c("thermalSensitivity", a00.model3VarsB)], nbest = 1, nvmax = length(a00.model3VarsB), force.in = NULL, force.out = NULL, method = "exhaustive")
a00.summaryBestSubsetB <- summary(a00.bestSubsetB)
as.data.frame(a00.summaryBestSubsetB$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(a00.summaryBestSubsetB$adjr2) #4
# What are the best predictors
a00.summaryBestSubsetB$which[4, ]
# SLOPE, h2oRdDens, h2oHiCascP, AgricultRC
# Create linear regression w/ thermal sensitivity as RV & EVs recommended by leaps 
a00.model4B <- lm(thermalSensitivity ~ SLOPE + h2oRdDens + h2oHiCascP + AgricultRC, data = SortedO_D_TS_EVs2021)
a00.model5B <- lm.beta(a00.model4B)
# Summary with standarized coefficients
summary(a00.model5B) # 3/4 SIG w/ adj-R-squared 0.6363 - same model
#--------------------------------
# MODEL C)
# Removing BurnRCA & RoadsBuf
a00.model3C <- lm(thermalSensitivity ~ SLOPE + h2oLakesPe + h2oRdDens + h2oHiCascP + AgricultRC + WetlandsRC + HiCascBuf + Azimuth + AbsAzimuth, data = SortedO_D_TS_EVs2021)
# Define landscape variables from model3
a00.model3VarsC <- c("SLOPE", "h2oLakesPe", "h2oRdDens", "h2oHiCascP", "AgricultRC", "WetlandsRC", "HiCascBuf", "Azimuth", "AbsAzimuth")

# Run regsubsets() on all model3 landscape variables
a00.bestSubsetC <-
    regsubsets(thermalSensitivity~., data =SortedO_D_TS_EVs2021[ , c("thermalSensitivity", a00.model3VarsC)], nbest = 1, nvmax = length(a00.model3VarsC), force.in = NULL, force.out = NULL, method = "exhaustive")
a00.summaryBestSubsetC <- summary(a00.bestSubsetC)
as.data.frame(a00.summaryBestSubsetC$outmat)
# Ran leaps through dataset -> what recommended # of predictors to use
which.max(a00.summaryBestSubsetC$adjr2) #4
# What are the best predictors
a00.summaryBestSubsetC$which[4, ]
# SLOPE, h2oRdDens, h2oHiCascP, AgricultRC
# Create linear regression w/ thermal sensitivity as RV & EVs recommended by leaps 
a00.model4C <- lm(thermalSensitivity ~ SLOPE + h2oRdDens + h2oHiCascP + AgricultRC, data = SortedO_D_TS_EVs2021)
a00.model5C <- lm.beta(a00.model4C)
# Summary with standarized coefficients
summary(a00.model5C) # 3/4 SIG w/ adj-R-squared 0.6363 - same model
