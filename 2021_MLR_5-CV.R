# Finding training RMSE using original MLR & completed spatial 5-fold CV on geographically stratified random sample (N to S randomly distributed)
# 5-fold CV: trains on 4 folds of 58 sites & predicts remaining fold of 14 sites - each fold spans entire basin from N to S
#---------------------------------
# Plot residuals (O-P TS) on a map 
## ^ if geographic patterns may suggest missing variables
#-------------------------------

## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# Load package
library(tidyverse)

# Reading SortedTSAndEVs2021.csv file - sites sorted from northmost to southmost 
sortedData <- read.csv("results/2021/SortedTSAndEVs2021.csv")
saveRDS(sortedData, "results/2021/RDS/SortedTSandEVs2021.RDS")

# Check sample size
nrow(sortedData) # Should be 72 sites
#----------------------------------------------------------------------
# MLR model on full dataset - training RMSE - uses SAME data model was fit on
mlr <- lm(thermalSensitivity ~ h2oHiCascP + Shrub21 + BurnRCA + h2oWetland + SLOPE, data = sortedData)
summary(mlr)
# Get predicted values from full MLR
predictedMLR <- predict(mlr)
# Compute residuals
residualsMLR <- sortedData$thermalSensitivity - predictedMLR
# Calculate training RMSE
rmse_training <- sqrt(mean(residualsMLR^2))
print(paste("Training RMSE (overly optimistic - uses SAME data model was fit on): ", round(rmse_training, 4)))
#------------------------
# Geographic stratification using sorted data (N to S)
# Use sorted data to distribute the N to S sites evenly across folds to make systematic & random sample 
# Would put Sites 1, 2, 3, 4, 5 in Folds 1, 2, 3, 4, 5 then Sites 6, 7, 8, 9, 10 into Folds 1, 2, 3, 4, 5, etc. 
sortedData <- sortedData %>%
  arrange(index) %>%
  mutate(fold_number = paste0("Fold", ((index - 1) %% 5) + 1))
#-------------------------
# Completing spatial 5-fold CV:
# Create separate training datasets for each fold 
train_fold1 <- filter(sortedData, fold_number != "Fold1")  # Train on folds 2,3,4,5
train_fold2 <- filter(sortedData, fold_number != "Fold2")  # Train on folds 1,3,4,5
train_fold3 <- filter(sortedData, fold_number != "Fold3")  # Train on folds 1,2,4,5
train_fold4 <- filter(sortedData, fold_number != "Fold4")  # Train on folds 1,2,3,5
train_fold5 <- filter(sortedData, fold_number != "Fold5")  # Train on folds 1,2,3,4

# Create linear regression models for each fold 
model_fold1 <- lm(thermalSensitivity ~ h2oHiCascP + Shrub21 + BurnRCA + h2oWetland + SLOPE, data = train_fold1)
model_fold2 <- lm(thermalSensitivity ~ h2oHiCascP + Shrub21 + BurnRCA + h2oWetland + SLOPE, data = train_fold2)
model_fold3 <- lm(thermalSensitivity ~ h2oHiCascP + Shrub21 + BurnRCA + h2oWetland + SLOPE, data = train_fold3)
model_fold4 <- lm(thermalSensitivity ~ h2oHiCascP + Shrub21 + BurnRCA + h2oWetland + SLOPE, data = train_fold4)
model_fold5 <- lm(thermalSensitivity ~ h2oHiCascP + Shrub21 + BurnRCA + h2oWetland + SLOPE, data = train_fold5)

# Queue testing data for each fold 
test_fold1 <- filter(sortedData, fold_number == "Fold1")
test_fold2 <- filter(sortedData, fold_number == "Fold2")
test_fold3 <- filter(sortedData, fold_number == "Fold3")
test_fold4 <- filter(sortedData, fold_number == "Fold4")
test_fold5 <- filter(sortedData, fold_number == "Fold5")

# Predict model using testing data for each fold 
predictions_fold1 <- predict(model_fold1, newdata = test_fold1) # Predict model 1 using folds 2, 3, 4, 5
predictions_fold2 <- predict(model_fold2, newdata = test_fold2) # Predict model 2 using folds 1, 3, 4, 5
predictions_fold3 <- predict(model_fold3, newdata = test_fold3) # Predict model 3 using folds 1, 2, 4, 5
predictions_fold4 <- predict(model_fold4, newdata = test_fold4) # Predict model 4 using folds 1, 2, 3, 5
predictions_fold5 <- predict(model_fold5, newdata = test_fold5) # Predict model 5 using folds 1, 2, 3, 4

# Calculate RMSE for each fold
rmse_fold1 <- sqrt(mean((test_fold1$thermalSensitivity - predictions_fold1)^2))
rmse_fold2 <- sqrt(mean((test_fold2$thermalSensitivity - predictions_fold2)^2))
rmse_fold3 <- sqrt(mean((test_fold3$thermalSensitivity - predictions_fold3)^2))
rmse_fold4 <- sqrt(mean((test_fold4$thermalSensitivity - predictions_fold4)^2))
rmse_fold5 <- sqrt(mean((test_fold5$thermalSensitivity - predictions_fold5)^2))

# Print RMSE for each fold
print("Cross-validation results:")
print(paste("RMSE for Fold 1:", round(rmse_fold1, 4)))
print(paste("RMSE for Fold 2:", round(rmse_fold2, 4)))
print(paste("RMSE for Fold 3:", round(rmse_fold3, 4)))
print(paste("RMSE for Fold 4:", round(rmse_fold4, 4)))
print(paste("RMSE for Fold 5:", round(rmse_fold5, 4)))

# Aggregate to create average RMSE across folds - mean CV RMSE
cv_rmse <- (rmse_fold1 + rmse_fold2 + rmse_fold3 + rmse_fold4 + rmse_fold5) / 5 
print(paste("Mean Cross-Validation RMSE:", round(cv_rmse, 4)))
print(paste("On average, our model's predictions of thermalSensitivity are off from the actual values by 0.0894"))
#---------------------------------------------------------
# Compare training vs CV performance:
# RMSE 
print(paste("Training RMSE:", round(rmse_training, 4)))
print(paste("CV RMSE:", round(cv_rmse, 4)))
print(paste("Difference in CV RMSE - Training RMSE:", round(cv_rmse - rmse_training, 4)))

print("This 0.0053 difference in Training RMSE and CV RMSE means there is minimal overfitting and MLR generalizes to new sites across CRB well.")

# Calculate R-squared for 5-fold CV 
# Computing SSR - Sum of Squared Residuals
cv_ssr <- sum(c((test_fold1$thermalSensitivity - predictions_fold1)^2, (test_fold2$thermalSensitivity - predictions_fold2)^2, (test_fold3$thermalSensitivity - predictions_fold3)^2, (test_fold4$thermalSensitivity - predictions_fold4)^2, (test_fold5$thermalSensitivity - predictions_fold5)^2))
# Computing SST - Total Sum of Squared
cv_sst <- sum((sortedData$thermalSensitivity - mean(sortedData$thermalSensitivity))^2)
cv_rSquared <- 1 - (cv_ssr / cv_sst)

print(paste("Cross-validation R-squared:", round(cv_rSquared, 3)))
print(paste("Training R-squared:", round(summary(mlr)$r.squared, 3)))

print("This means our 5 landscape covariates explain ~59% of variation in thermal sensitivity when predicting new sites - it can generalize.")
#---------------------------------
# Graphs: predicted vs. actual for each fold 
library(ggplot2)
library(patchwork)

# Create dataframes and plots for each fold
df1 <- data.frame(actual = test_fold1$thermalSensitivity, predicted = predictions_fold1)
plot1 <- ggplot(df1, aes(x = predicted, y = actual)) + 
  geom_point(alpha = 0.7) + 
  geom_abline(intercept = 0, slope = 1, color = "#f7aa58", linewidth = 1) +
  ggtitle("Spatial Cross-Validation") + 
  labs(subtitle = paste('Fold 1 - RMSE:', round(rmse_fold1, 4))) +
  xlab("Predicted Thermal Sensitivity") + 
  ylab("Actual Thermal Sensitivity") +
  theme_minimal()

df2 <- data.frame(actual = test_fold2$thermalSensitivity, predicted = predictions_fold2)
plot2 <- ggplot(df2, aes(x = predicted, y = actual)) + 
  geom_point(alpha = 0.7) + 
  geom_abline(intercept = 0, slope = 1, color = "#f7aa58", linewidth = 1) +
  ggtitle("Spatial Cross-Validation") + 
  labs(subtitle = paste('Fold 2 - RMSE:', round(rmse_fold2, 4))) +
  xlab("Predicted Thermal Sensitivity") + 
  ylab("Actual Thermal Sensitivity") +
  theme_minimal()

df3 <- data.frame(actual = test_fold3$thermalSensitivity, predicted = predictions_fold3)
plot3 <- ggplot(df3, aes(x = predicted, y = actual)) + 
  geom_point(alpha = 0.7) + 
  geom_abline(intercept = 0, slope = 1, color = "#f7aa58", linewidth = 1) +
  ggtitle("Spatial Cross-Validation") + 
  labs(subtitle = paste('Fold 3 - RMSE:', round(rmse_fold3, 4))) +
  xlab("Predicted Thermal Sensitivity") + 
  ylab("Actual Thermal Sensitivity") +
  theme_minimal()

df4 <- data.frame(actual = test_fold4$thermalSensitivity, predicted = predictions_fold4)
plot4 <- ggplot(df4, aes(x = predicted, y = actual)) + 
  geom_point(alpha = 0.7) + 
  geom_abline(intercept = 0, slope = 1, color = "#f7aa58", linewidth = 1) +
  ggtitle("Spatial Cross-Validation") + 
  labs(subtitle = paste('Fold 4 - RMSE:', round(rmse_fold4, 4))) +
  xlab("Predicted Thermal Sensitivity") + 
  ylab("Actual Thermal Sensitivity") +
  theme_minimal()

df5 <- data.frame(actual = test_fold5$thermalSensitivity, predicted = predictions_fold5)
plot5 <- ggplot(df5, aes(x = predicted, y = actual)) + 
  geom_point(alpha = 0.7) + 
  geom_abline(intercept = 0, slope = 1, color = "#f7aa58", linewidth = 1) +
  ggtitle("Spatial Cross-Validation") + 
  labs(subtitle = paste('Fold 5 - RMSE:', round(rmse_fold5, 4))) +
  xlab("Predicted Thermal Sensitivity") + 
  ylab("Actual Thermal Sensitivity") +
  theme_minimal()

# Arrange all 5 plots with overall title
combined_plot <- (plot1 + plot2 + plot3) / (plot4 + plot5)
combined_plot + plot_annotation(
  title = "5-Fold Spatial Cross-Validation Results",
  subtitle = paste("Mean CV RMSE:", round(cv_rmse, 4), "| Training RMSE:", round(rmse_training, 4))
)
#------------------------
