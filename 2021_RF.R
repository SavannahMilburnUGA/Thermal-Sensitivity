# Using a RF model to validate spatial influences from MLR - RF can handle complex interactions - Do non-linear relationships exist that MLR is not accounting for?
# Perform HPO using h2o package & random grid search strategy to find good RF model using all 40 landscape covariates 

## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()
#-------------------------------------------------------------------------------------------------------------------------
# Train RF model by fitting RF model using optimized hyperparameters on 4 folds
# Calculate OOB error as internal validation metric
# Generate predictions for held-out test fold

# Load packages 
library(dplyr)
library(ggplot2)
library(ranger) # RF model
# install.packages("h2o")
library(h2o) # HPO
#-------------------------------------------------------------------------------------------------------------------------
## Basic algorithm for RF: 
#1) select # of trees to build: generate bootstrap sample of original data
#2) Grow a regression tree to bootstrapped data & for each split: select m_try VARBS at RANDOM from all p variables
#3) Pick best variable/split-point among m_try
#4) Split node into 2 child nodes
#5) Use typical stopping criteria to determine when tree complete but do NOT prune
# When m_try = p: ALG is EQUIVALENT to BAGGING DECISION TREES
# Default values good: good OOB performance but can TUNE HYPERPARAMETERS
# ranger sets m_try to floor(sqrt(# features)) BUT for REGRESSION: start w/ m_try = floor( # features/3)
# Set respect.unordered.factors = "order"
#-------------------------------------------------------------------------------------------------------------------------
# Reading SortedTSAndEVs2021.csv file - sites sorted from northmost to southmost 
sortedData <- read.csv("results/2021/SortedTSAndEVs2021.csv")
saveRDS(sortedData, "results/2021/RDS/SortedTSandEVs2021.RDS")
# Including Solar as an EV - all 40 landscape covariates
EVs2021 <- c("SLOPE", "Solar", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip")
# Subset to only landscape EVs + RV: thermalSensitivity
rfData <- sortedData[, c("thermalSensitivity", EVs2021)]
#-------------------------------------------------------------------------------------------------------------------------
# Training a default RF model - OOB performance : uses mtry = # features/3 for regression & sets categorical varaibles to order
rf1 <- ranger(thermalSensitivity ~ ., data = rfData, mtry = floor(40 / 3), importance = "permutation", respect.unordered.factors = "order", seed = 123)
# Compute OOB RMSE 
(default_rmse <- sqrt(rf1$prediction.error)) # 0.0677614 
#-------------------------------------------------------------------------------------------------------------------------
# Hyperparameters: # of tree in RF, # of features to consider at any given split: m_try, complexity of each tree, sampling scheem, splitting rule for tree construction
## m_try usually largest impact on predictive accuracy -> always be tuned
## tree complexity & sampling scheme usually marginal impact
## splitting rule tends to have smallest impact on prediction but can impact computation efficiency
#----------------------------
# # of tree: start w/ 10*# of features - but adjusting other hyperparameters like m_try & node side - fewer/more trees may be needed
# More trees provide more robust & stable error estimates & varibale importance measures but computation time increases linearly w/ # of trees
## Plot # of trees vs. OOB Error (RMSE) - 10p typically ensures convergence for error estimate
#----------------------------
# m_try = controls split-variable randomization of RF 
# When fewer relevant predictors (noisy data), higher value of m_try tends to perform better bc. 
## more likely to select those features w/ strongest signal 
## Plot # of trees vs. OOB Error (RMSE) - start w/ 5 evenly spaced values of m_try across 2-p range centered at recommended default
#----------------------------
# tree complexity - node size, max depth, max # terminal nodes, required node side to allow additional splits
## Node size most common hyperparameter - 5 for regression
## If have many noisy predictors & higher m_try values best -> increase node size
#----------------------------
## Sampling scheme: most common is bootstrapping: 100% of observations sampled w/ replacement
### Each bootstrap copy has same size as original training data
#----------------------------
## Split rule: minimizes SSE in regression
## Conditional inference trees implement alternative splitting mechanism to reduce variable selection bias - but take longer to train
#----------------------------
# Tuning strategies: full Cartesian grid search - assess every combo of hyperparameters of interest vs. HPO
#-------------------------------------------------------------------------------------------------------------------------
#1) Hyperparameter optimization for RF model (HPO)
## Test different RF parameter combos (mtry, node size, etc.) using OOB (RMSE) error
## Find best RF model parameters so can compare to MLR
### Use h2o grid search w/ OOB error on all 72 sites - uses bootstrapping
# h2o package: provides random grid search allowing you to jump from 1 random combo to another & provides early stopping rules to stop grid serach once certain condition is met (accuracy stopped improving by certain amount)
## Won't find optimal model but will find a very good model 
#-------------------------------------------------------------------------------------------------------------------------
# Initiate h2o session
h2o.no_progress()
h2o.init(max_mem_size = "2g") # Small since N = 72
# Remove any existing grids to avoid conflicts
h2o.removeAll() 
# Convert training data to valid h2o object
train_h2o <- as.h2o(rfData)
# Set response column to thermal sensitivity
response <- "thermalSensitivity"
# Set predictor names
predictors <- setdiff(colnames(rfData), response)
#-------------------------------------------------------------------------------------------------------------------------
# Executing a grid search in h2o - stop if none of last 10 models have managed to have a 0.1% improvement in MSE compared to best model before
# If continue to find improvements: cut grid search off after 5 minutes
# Hyperparameter grid
hyper_grid <- list(mtries = floor(40 * c(0.05, 0.15, 0.25, 0.333, 0.4)), min_rows = c(1, 3, 5, 7), max_depth = c(10, 15, 20, 25), sample_rate = c(.50, .63, .7, .8))
# Print the hyperparameter grid
print("Hyperparameter Grid:")
print(paste("mtries:", paste(hyper_grid$mtries, collapse = ", ")))
print(paste("min_rows:", paste(hyper_grid$min_rows, collapse = ", ")))
print(paste("max_depth:", paste(hyper_grid$max_depth, collapse = ", ")))
print(paste("sample_rate:", paste(hyper_grid$sample_rate, collapse = ", ")))
print(paste("Total combinations:", length(hyper_grid$mtries) * length(hyper_grid$min_rows) * length(hyper_grid$max_depth) * length(hyper_grid$sample_rate)))
# Random grid search strategy: stop if improvement is < 0.5% in RMSE over last 8 models or continute to find improvements until 3 minutes
search_criteria <- list(strategy = "RandomDiscrete", stopping_metric = "rmse", stopping_tolerance = 0.005, stopping_rounds = 8, max_runtime_secs = 60*3)
# Print random grid search strategy
print("Random Grid Search Strategy:")
print("- Random discrete search")
print("- Stop if RMSE improvement < 0.5% over last 8 models")
print("- Maximum 3 minutes runtime") 
# Perform grid search - 400 trees per model - stop tree growth if no improvement in RMSE (does NOT improve RMSE by 0.5%) in last 10 trees
random_grid <- h2o.grid(algorithm = "randomForest", grid_id = "rf_random_grid", x = predictors, y = response, training_frame = train_h2o, hyper_params = hyper_grid, ntrees = 40 * 10, seed = 123, stopping_metric = "RMSE", stopping_rounds = 10, stopping_tolerance = 0.005, search_criteria = search_criteria)
print("Grid search completed!")
# Get results and sort by model performance metric specified 
random_grid_rf_results <- h2o.getGrid(grid_id = "rf_random_grid", sort_by = "mse", decreasing = FALSE)
print("Grid search results: ")
random_grid_rf_results
#-------------------------------------------------------------------------------------------------------------------------
# Extract best RF model
best_RF_model <- h2o.getModel(random_grid_rf_results@model_ids[[1]])
best_rmse <- h2o.rmse(best_RF_model, train = TRUE)

print(paste("Best model RMSE:", round(best_rmse, 6)))

# Get best hyperparameters
best_params <- best_RF_model@parameters
print("Optimal hyperparameters from best RF model:")
cat("mtry:", best_RF_model@parameters$mtries, "\n")
cat("sample_rate:", best_RF_model@parameters$sample_rate, "\n")
print("Complete model summary:")
print(best_RF_model)
#-------------------------------------------------------------------------------------------------------------------------
# Create comparison of top models
top_models <- h2o.getGrid(grid_id = "rf_random_grid", sort_by = "rmse", decreasing = FALSE)

# Extract info summary for top 5 models
print("Top 5 RF Models Performance:")
print(paste("Number of models available:", length(top_models@model_ids)))

for(i in 1:min(5, length(top_models@model_ids))) {
  tryCatch({
    model <- h2o.getModel(top_models@model_ids[[i]])
    rmse <- h2o.rmse(model, train = TRUE)
    
    # Get available parameters safely
    mtries_val <- ifelse(is.null(model@parameters$mtries), "NA", model@parameters$mtries)
    sample_rate_val <- ifelse(is.null(model@parameters$sample_rate), "NA", model@parameters$sample_rate)
    
    cat(sprintf("Model %d: RMSE = %.6f, mtries = %s, sample_rate = %s\n", 
                i, rmse, mtries_val, sample_rate_val))
    
    # Print model summary to see all hyperparameters
    cat("Model", i, "summary:\n")
    print(model@model$model_summary)
    cat("\n")
    
  }, error = function(e) {
    cat("Error with model", i, ":", e$message, "\n")
  })
}
#-------------------------------------------------------------------------------------------------------------------------
# Save best hyperparameters for spatial 5-fold CV 
best_hyperparams <- list(mtry = best_params$mtry, min_rows = best_params$min_rows, max_depth = best_params$max_depth, sample_rate = best_params$sample_rate, ntrees = 40 * 10)

# Save for use in spatial CV
saveRDS(best_hyperparams, "results/2021/RF/optimal_rf_hyperparams.RDS")
print("Optimal hyperparameters saved for spatial cross-validation")

# Shut down h2o instance
h2o.shutdown(prompt = TRUE)
#-------------------------------------------------------------------------------------------------------------------------