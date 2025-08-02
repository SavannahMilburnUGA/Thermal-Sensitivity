# Perform same spatial 5-fold CV on RF model w/ optimal hyperparameteres from 2021_RF.R - can RF model predict at unseen sites?
### For same folds (1-5): train RF w/ optimal hyperparameters on 4 folds of 58 sites
### Predict held-out fold: fold 5 of 14 sites & calculate RMSE 

## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()
#-------------------------------------------------------------------------------------------------------------------------
# Reading SortedTSAndEVs2021.csv file - sites sorted from northmost to southmost 
sortedData <- read.csv("results/2021/SortedTSAndEVs2021.csv")
saveRDS(sortedData, "results/2021/RDS/SortedTSandEVs2021.RDS")

# Check sample size
nrow(sortedData) # Should be 72 sites
# Including Solar as an EV
EVs2021 <- c("SLOPE", "Solar", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip")
# Subset to only landscape EVs + RV: thermalSensitivity
rfData <- sortedData[, c("thermalSensitivity", EVs2021)]
#-------------------------------------------------------------------------------------------------------------------------
# MLR & spatial 5-fold CV on MLR results: RMSE, R-squared

# For model using SAME data MLR model was fit on - overly optimistic
training_RMSE <- 0.084
training_R_Squared <- 0.651 

# Spatial 5-fold CV RMSEs for MLR model using 5 covariates
mlr_fold_1_RMSE <- 0.0798
mlr_fold_2_RMSE <- 0.109
mlr_fold_3_RMSE <- 0.0866
mlr_fold_4_RMSE <- 0.0676
mlr_fold_5_RMSE <- 0.1039
mlr_CV_RMSE <- 0.0894
# On average, our model's predictions of thermalSensitivity are off from the actual values by 0.0894
# Difference in MLR CV RMSE - Training RMSE: 0.0053
# This 0.0053 difference in Training RMSE and CV RMSE means there is minimal overfitting and the MLR model generalizes to new sites across CRB well.

mlr_CV_R_Squared <- 0.593
# This means our 5 landscape covariates explain ~59% of variation in thermal sensitivity when predicting new sites - it can generalize.
#-------------------------------------------------------------------------------------------------------------------------
# Load packages
library(dplyr)
library(ranger)
library(ggplot2)
library(patchwork)
library(tidyverse)
#-------------------------------------------------------------------------------------------------------------------------
# Geographic stratification using sorted data (N to S)
# Use sorted data to distribute the N to S sites evenly across folds to make systematic & random sample 
# Would put Sites 1, 2, 3, 4, 5 in Folds 1, 2, 3, 4, 5 then Sites 6, 7, 8, 9, 10 into Folds 1, 2, 3, 4, 5, etc. 
sortedData <- sortedData %>%
  arrange(index) %>%
  mutate(fold_number = paste0("Fold", ((index - 1) %% 5) + 1))
#-------------------------------------------------------------------------------------------------------------------------
# Use the optimal hyperparameters found by h2o grid search
optimal_hyperparams <- list(mtry = 13, sample.fraction = 0.7, min.node.size = 5, num.trees = 400, importance = "permutation",replace = FALSE)

print("Using optimal hyperparameters from h2o grid search:")
print(optimal_hyperparams)
#-------------------------------------------------------------------------------------------------------------------------
# Completing spatial 5-fold CV:
# Create separate training datasets for each fold 
train_fold1 <- filter(sortedData, fold_number != "Fold1")  # Train on folds 2,3,4,5
train_fold2 <- filter(sortedData, fold_number != "Fold2")  # Train on folds 1,3,4,5
train_fold3 <- filter(sortedData, fold_number != "Fold3")  # Train on folds 1,2,4,5
train_fold4 <- filter(sortedData, fold_number != "Fold4")  # Train on folds 1,2,3,5
train_fold5 <- filter(sortedData, fold_number != "Fold5")  # Train on folds 1,2,3,4

# Create RF models for each fold 
rf_model_fold1 <- ranger(thermalSensitivity ~ ., data = train_fold1[, c("thermalSensitivity", EVs2021)], mtry = 13, num.trees = 400, sample.fraction = 0.7, min.node.size = 5, importance = "permutation", replace = FALSE, seed = 123, respect.unordered.factors = "order")
rf_model_fold2 <- ranger(thermalSensitivity ~ ., data = train_fold2[, c("thermalSensitivity", EVs2021)], mtry = 13, num.trees = 400, sample.fraction = 0.7, min.node.size = 5, importance = "permutation", replace = FALSE, seed = 123, respect.unordered.factors = "order")
rf_model_fold3 <- ranger(thermalSensitivity ~ ., data = train_fold3[, c("thermalSensitivity", EVs2021)], mtry = 13, num.trees = 400, sample.fraction = 0.7, min.node.size = 5, importance = "permutation", replace = FALSE, seed = 123, respect.unordered.factors = "order")
rf_model_fold4 <- ranger(thermalSensitivity ~ ., data = train_fold4[, c("thermalSensitivity", EVs2021)], mtry = 13, num.trees = 400, sample.fraction = 0.7, min.node.size = 5, importance = "permutation", replace = FALSE, seed = 123, respect.unordered.factors = "order")
rf_model_fold5 <- ranger(thermalSensitivity ~ ., data = train_fold5[, c("thermalSensitivity", EVs2021)], mtry = 13, num.trees = 400, sample.fraction = 0.7, min.node.size = 5, importance = "permutation",replace = FALSE, seed = 123, respect.unordered.factors = "order")

# Queue testing data for each fold 
test_fold1 <- filter(sortedData, fold_number == "Fold1")
test_fold2 <- filter(sortedData, fold_number == "Fold2")
test_fold3 <- filter(sortedData, fold_number == "Fold3")
test_fold4 <- filter(sortedData, fold_number == "Fold4")
test_fold5 <- filter(sortedData, fold_number == "Fold5")

# Predict RF model using testing data for each fold 
rf_predictions_fold1 <- predict(rf_model_fold1, data = test_fold1)$predictions # Predict model 1 using folds 2, 3, 4, 5
rf_predictions_fold2 <- predict(rf_model_fold2, data = test_fold2)$predictions # Predict model 2 using folds 1, 3, 4, 5
rf_predictions_fold3 <- predict(rf_model_fold3, data = test_fold3)$predictions # Predict model 3 using folds 1, 2, 4, 5
rf_predictions_fold4 <- predict(rf_model_fold4, data = test_fold4)$predictions # Predict model 4 using folds 1, 2, 3, 5
rf_predictions_fold5 <- predict(rf_model_fold5, data = test_fold5)$predictions # Predict model 5 using folds 1, 2, 3, 4

# Calculate RMSE for each fold
rf_rmse_fold1 <- sqrt(mean((test_fold1$thermalSensitivity - rf_predictions_fold1)^2))
rf_rmse_fold2 <- sqrt(mean((test_fold2$thermalSensitivity - rf_predictions_fold2)^2))
rf_rmse_fold3 <- sqrt(mean((test_fold3$thermalSensitivity - rf_predictions_fold3)^2))
rf_rmse_fold4 <- sqrt(mean((test_fold4$thermalSensitivity - rf_predictions_fold4)^2))
rf_rmse_fold5 <- sqrt(mean((test_fold5$thermalSensitivity - rf_predictions_fold5)^2))

# Print RMSE for each fold
print("Cross-validation results:")
print(paste("RF RMSE:", round(rf_rmse_fold1, 4)))
print(paste("RF RMSE:", round(rf_rmse_fold2, 4)))
print(paste("RF RMSE:", round(rf_rmse_fold3, 4)))
print(paste("RF RMSE:", round(rf_rmse_fold4, 4)))
print(paste("RF RMSE:", round(rf_rmse_fold5, 4)))

# Aggregate to create average RMSE across folds - mean CV RMSE
rf_cv_rmse <- (rf_rmse_fold1 + rf_rmse_fold2 + rf_rmse_fold3 + rf_rmse_fold4 + rf_rmse_fold5) / 5 
print(paste("Mean Cross-Validation RMSE:", round(rf_cv_rmse, 4)))
print(paste("On average, our model's predictions of thermalSensitivity are off from the actual values by 0.0681 compared to the old MLR 0.0894"))
#---------------------------------------------------------
# Compare training vs CV performance:
# RMSE 
# For model using SAME data MLR model was fit on - overly optimistic
print(paste("Training RMSE:", round(training_RMSE, 4)))

improvement <- (mlr_CV_RMSE - rf_cv_rmse) / mlr_CV_RMSE * 100
print(paste("RF CV RMSE improvement over MLR CV RMSE:", round(improvement, 2), "%"))
print(paste("RF CV RMSE:", round(rf_cv_rmse, 4)))

print(paste("Difference in RF CV RMSE - Training RMSE:", round(rf_cv_rmse - training_RMSE, 4)))
print("This 0.0159 difference in Training RMSE and CV RMSE means there is minimal overfitting and RF generalizes to new sites across CRB well")

# Calculate R-squared for 5-fold CV 
# Computing SSR - Sum of Squared Residuals
rf_cv_ssr <- sum(c((test_fold1$thermalSensitivity - rf_predictions_fold1)^2, (test_fold2$thermalSensitivity - rf_predictions_fold2)^2, (test_fold3$thermalSensitivity - rf_predictions_fold3)^2, (test_fold4$thermalSensitivity - rf_predictions_fold4)^2, (test_fold5$thermalSensitivity - rf_predictions_fold5)^2))
# Computing SST - Total Sum of Squared
cv_sst <- sum((sortedData$thermalSensitivity - mean(sortedData$thermalSensitivity))^2)
rf_cv_rSquared <- 1 - (rf_cv_ssr / cv_sst)

print(paste("RF CV R-squared:", round(rf_cv_rSquared, 3)))
print(paste("MLR CV R-squared:", round(mlr_CV_R_Squared, 3)))
print("This means our 5 landscape covariates explain ~76% of variation in thermal sensitivity when predicting new sites - it can generalize.")
#-------------------------------------------------------------------------------------------------------------------------
# Get quick variable importance from RF model from fold 1 - need to recheck
rf_importance <- importance(rf_model_fold1)
print("Top 8 most important variables:")
print(head(sort(rf_importance, decreasing = TRUE), 8))
#-------------------------------------------------------------------------------------------------------------------------