# RF 1

# Simple RF using 5 sig landscape covariates from MLR
# Simple GW-RF usign 5 sig landscape covariates from MLR

# Load required libraries
# install.packages("randomForest")
library(randomForest)  # Basic RF
# install.packages("SpatialML")
library(SpatialML)     # GW-RF
library(ggplot2)       # Plotting
library(dplyr)         # Data manipulation

# Seed for reproducibility
set.seed(123)

# Load data
data <- read.csv("results/2021/TSAndEVs2021.csv")

# Define your variables
response_var <- "thermalSensitivity"
predictors <- c("SLOPE", "h2oHiCascP", "h2oWetland", "Shrub21", "BurnRCA")
coord_vars <- c("x", "y")  # Replace with your actual coordinate column names

# Create clean dataset with only needed variables
model_data <- data %>%
  select(all_of(c(response_var, predictors, coord_vars))) %>%
  na.omit()  # Remove any rows with missing values
cat("Clean dataset:", nrow(model_data), "sites with complete data\n")

#-------------------------------------------------------
# Training set & testing set 
# Create 70/30 train/test split (conservative for 70 sites)
train_size <- floor(0.7 * nrow(model_data))
train_indices <- sample(seq_len(nrow(model_data)), size = train_size)

train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

cat("Training set:", nrow(train_data), "sites\n")
cat("Test set:", nrow(test_data), "sites\n")

# Basic RF w/ 5 landscope covariates
cat("BUILDING BASIC RANDOM FOREST MODEL\n")
# Create formula
rf_formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))
print(rf_formula)
# Fit basic RF model - 500 as std # of trees, sqrt(5) = 2 for regression, calculate varb importance 
rf_model <- randomForest(formula = rf_formula, data = train_data, ntree = 500, mtry = 2, importance = TRUE)
# Print model summary
print(rf_model)

# Make predictions on test set
rf_predictions <- predict(rf_model, newdata = test_data)
# Calculate performance metrics
rf_rmse <- sqrt(mean((rf_predictions - test_data[[response_var]])^2))
rf_mae <- mean(abs(rf_predictions - test_data[[response_var]]))
rf_r2 <- cor(rf_predictions, test_data[[response_var]])^2

cat("\nBASIC RF w/ 5 landscape covariates PERFORMANCE:\n")
cat("RMSE:", round(rf_rmse, 4), "\n")
cat("MAE: ", round(rf_mae, 4), "\n")
cat("R²:  ", round(rf_r2, 4), "\n")

#-----------------
# Simple GW RF using 5 landscape covariates from MLR
cat("BUILDING GEOGRAPHICALLY WEIGHTED RF MODEL\n")

# Prepare coordinates for training data
train_coords <- train_data[, coord_vars]

# Fit GW-RF model - bandwidth choice important but simple right now - optimize later - using std 500 trees again & 2 as mtry
gwrf_model <- grf( formula = rf_formula, dframe = train_data, bw = 50, ntree = 500, mtry = 2, kernel = "adaptive", coords = train_coords)

# Print global model summary (for comparison)
cat("Global model summary:\n")
cat("Global R²:", round(mean(gwrf_model$Global.Model$rsq), 4), "\n")
cat("Global MSE:", round(mean(gwrf_model$Global.Model$mse), 4), "\n")

# Make predictions on test set - use local weights (not global weights)
test_coords <- test_data[, coord_vars]
gwrf_predictions <- predict.grf(gwrf_model, test_data, x.var.name = coord_vars[1],  y.var.name = coord_vars[2],  local.w = 1, global.w = 0)
# Calculate performance metrics
gwrf_rmse <- sqrt(mean((gwrf_predictions - test_data[[response_var]])^2))
gwrf_mae <- mean(abs(gwrf_predictions - test_data[[response_var]]))
gwrf_r2 <- cor(gwrf_predictions, test_data[[response_var]])^2

cat("\nGW-RF PERFORMANCE:\n")
cat("RMSE:", round(gwrf_rmse, 4), "\n")
cat("MAE: ", round(gwrf_mae, 4), "\n")
cat("R²:  ", round(gwrf_r2, 4), "\n")

#----------------------
# Comparing models
cat("MODEL COMPARISON RESULTS\n")
# Create comparison table
comparison <- data.frame(Model = c("Basic RF", "GW-RF"),RMSE = c(round(rf_rmse, 4), round(gwrf_rmse, 4)),MAE = c(round(rf_mae, 4), round(gwrf_mae, 4)),R_squared = c(round(rf_r2, 4), round(gwrf_r2, 4)))
print(comparison)

# Calculate improvement
rmse_improvement <- round(((rf_rmse - gwrf_rmse) / rf_rmse) * 100, 2)
r2_improvement <- round(((gwrf_r2 - rf_r2) / rf_r2) * 100, 2)

cat("\nPerformance Changes (GW-RF vs Basic RF):\n")
cat("RMSE change:", rmse_improvement, "%\n")
cat("R² change:  ", r2_improvement, "%\n")

if(rmse_improvement > 5) {
  cat("\n GW-RF shows meaningful improvement - spatial variation exists!\n")
} else {
  cat("\n Similar performance - relationships may be spatially consistent\n")
}

#-------------------
# Variable importance comparison
cat("VARIABLE IMPORTANCE COMPARISON\n")
# Basic RF variable importance
cat("Basic RF Variable Importance:\n")
# For Basic RF variable importance:
rf_importance <- rf_model$importance[, "%IncMSE"]
print(sort(rf_importance, decreasing = TRUE))

# GW-RF global variable importance  
cat("\nGW-RF Global Variable Importance:\n")
gwrf_importance <- gwrf_model$Global.Model$importance[, "%IncMSE"]
gwrf_importance_sorted <- sort(gwrf_importance, decreasing = TRUE)
print(round(gwrf_importance_sorted, 2))

#--------------
# Prediction comparison plots
# Create prediction comparison plot
plot_data <- data.frame(Observed = rep(test_data[[response_var]], 2),Predicted = c(rf_predictions, gwrf_predictions),Model = factor(rep(c("Basic RF", "GW-RF"), each = length(rf_predictions))))
p1 <- ggplot(plot_data, aes(x = Observed, y = Predicted, color = Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  facet_wrap(~Model) +
  labs(title = "Observed vs Predicted Thermal Sensitivity",
       x = "Observed", y = "Predicted") +
  theme_minimal() +
  theme(legend.position = "none")
print(p1)

# Residual plots
residuals_data <- data.frame(Predicted = c(rf_predictions, gwrf_predictions),Residuals = c(rf_predictions - test_data[[response_var]], gwrf_predictions - test_data[[response_var]]),Model = factor(rep(c("Basic RF", "GW-RF"), each = length(rf_predictions))))

p2 <- ggplot(residuals_data, aes(x = Predicted, y = Residuals, color = Model)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "loess", se = FALSE, size = 1) +
  facet_wrap(~Model) +
  labs(title = "Residual Plots",
       x = "Predicted", y = "Residuals") +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)


#------------------------------
cat("SUMMARY AND RECOMMENDATIONS\n")

cat("Models built with 5 landscape covariates:", paste(predictors, collapse = ", "), "\n")
cat("Training sites:", nrow(train_data), "\n")
cat("Test sites:", nrow(test_data), "\n\n")

if(rmse_improvement > 10) {
  cat("STRONG spatial effects detected:\n")
  cat("Thermal sensitivity relationships vary significantly across your basin\n")
  cat("Consider using GW-RF for final modeling\n")
  cat("Investigate spatial patterns in local variable importance\n")
} else if(rmse_improvement > 5) {
  cat("MODERATE spatial effects detected:\n") 
  cat("Some spatial variation in relationships exists\n")
  cat("GW-RF provides modest improvement\n")
  cat("Consider both models depending on research goals\n")
} else {
  cat("MINIMAL spatial effects detected:\n")
  cat("Thermal sensitivity relationships are relatively consistent across basin\n")
  cat("Basic RF is sufficient for most purposes\n")
  cat("Focus on improving global model with additional variables\n")
}

cat("\nNext steps:\n")
cat("Examine spatial patterns in GW-RF local importance (if meaningful improvement)\n") 
cat("Consider testing with additional landscape variables\n")
cat("Validate results with cross-validation\n")
cat("\nAnalysis complete!\n")