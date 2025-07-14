## Experimenting w/ MLR to find best criteria & EVs - NO SOLAR EV
## Running MLR using 2.4 section - initial model

# Clean house & remove saved files (keeping it clean)
# Remove all objects in workspace 
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# Load all data: TSAndEVs2021, full correlation matrix, all EVs
# Loading RDS of thermal sensitivities joined w/ EV values for 2021
TSAndEVs2021 <- readRDS("results/2021/RDS/TSandEVs2021.RDS")
# Check
nrow(TSAndEVs2021)
# Load full correlation matrix
fullCorrMatrix <- readRDS("results/2021/correlation/RDS/fullCorrMatrix.RDS")
# Define all EVs/landscape variables from Michael 
EVs2021 <- c("SLOPE", "Elev", "BFI", "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2", "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_","DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf","MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip")

#------------------------------------------------------------------------------------------------------------------------------------------------
# Load libraries
library(car) # VIFs
library(leaps) # select best MLR based on adj R^2
library(lm.beta) # extracting standardized beta coefficients
library(readr) # for write_csv
#-------------------------------------------------------------------------------------------------------------------
## Creating function that runs through 2.4 Section steps
generateMLRModel <- function(vifCutOff, corrCutOff, includedEVs, MLRModelName) {
    cat(sprintf("\n\n=================== GENERATING MODEL: %s ===================\n", MLRModelName))
    cat(sprintf("Parameters: VIF cutoff = %s, Correlation cutoff = %.1f\n", vifCutOff, corrCutOff))
    cat(sprintf("Starting variables: %d\n", length(includedEVs)))
    
    #1: BEFORE conducting MULTIVARIATE CORRELATIONS, 
    # TEST for MULTICOLLINEARITY among EVs by examining VIF factor
    # & REMOVING any VALUES > 5
    tryCatch({
        cat("\n--- Step 1: VIF Filtering ---\n")
        # Create linear regression w/ thermal sensitivity as RV & landscape varbs as EVs
        regression <- paste("thermalSensitivity ~", paste(includedEVs, collapse = " + "))
        b00.model1 <- lm(as.formula(regression), data = TSAndEVs2021)

        # Calculate VIF values
        b00.VIFs <- vif(b00.model1)
        # Saving VIF values
        write.csv(data.frame(Variable = names(b00.VIFs), VIF = b00.VIFs), 
                 sprintf("results/2021/MLR/VIFs/%s_VIFs.csv", MLRModelName))
        # Cutting out landscape EVs w/ high VIFs
        b00.EVsHighVIF <- names(b00.VIFs[b00.VIFs > vifCutOff])
        cat("Landscape EVs with VIFs above VIF cut off: ")
        if(length(b00.EVsHighVIF) > 0) {
            cat(paste(b00.EVsHighVIF, collapse = ", "), "\n")
        } else {
            cat("None\n")
        }

        b00.model2Vars <- includedEVs[!includedEVs %in% b00.EVsHighVIF]
        cat(sprintf("Remaining variables after VIF filtering: %d\n", length(b00.model2Vars)))
        
        if (length(b00.model2Vars) < 2) {
            cat("ERROR: Too few landscape EVs remaining after VIF filtering\n")
            return(NULL)
        }
    }, error = function(e) {
        cat("ERROR in #1 VIF filtering: ", e$message, "\n")
        return(NULL)
    })
    #-------------------------------------------------------------------------------------------------------------------
    #2&3: Remove highly correlated variables based on correlation cutoff
    cat("\n--- Step 2&3: Correlation Filtering ---\n")
    if(length(b00.model2Vars) > 1) {
        # Get correlation matrix for remaining variables
        b00.corrMatrix <- fullCorrMatrix$r[b00.model2Vars, b00.model2Vars]
        # Find correlations above the correlation cut off
        b00.cutOffPairs <- which(abs(b00.corrMatrix) >= corrCutOff & b00.corrMatrix != 1, arr.ind = TRUE)
        
        if(nrow(b00.cutOffPairs) > 0) {
            # Remove duplicates
            b00.noDuplicates <- b00.cutOffPairs[b00.cutOffPairs[,1] < b00.cutOffPairs[,2], , drop = FALSE]
            
            cat(sprintf("Found %d variable pairs with |correlation| >= %.1f\n", nrow(b00.noDuplicates), corrCutOff))
            
            b00.removedVars <- c()
            
            for(i in 1:nrow(b00.noDuplicates)) {
                var1 <- rownames(b00.corrMatrix)[b00.noDuplicates[i,1]]
                var2 <- colnames(b00.corrMatrix)[b00.noDuplicates[i,2]]
                
                # Skip if either variable already marked for removal
                if(var1 %in% b00.removedVars || var2 %in% b00.removedVars) next
                
                cat(sprintf("\nHigh correlation pair: %s & %s (r = %.3f)\n", 
                            var1, var2, b00.corrMatrix[var1, var2]))
                
                # Get VIFs for both variables (from step 1)
                b00.var1VIF <- b00.VIFs[var1]
                b00.var2VIF <- b00.VIFs[var2]
                
                # Get individual adjusted R^2
                b00.var1AdjRSquared <- summary(lm(thermalSensitivity ~ get(var1), data = TSAndEVs2021))$adj.r.squared
                b00.var2AdjRSquared <- summary(lm(thermalSensitivity ~ get(var2), data = TSAndEVs2021))$adj.r.squared
                
                cat(sprintf("  %s: VIF = %.2f, Adj R² = %.3f\n", var1, b00.var1VIF, b00.var1AdjRSquared))
                cat(sprintf("  %s: VIF = %.2f, Adj R² = %.3f\n", var2, b00.var2VIF, b00.var2AdjRSquared))
                
                
                VIFDiff <- abs(b00.var1VIF - b00.var2VIF)
                
                if(VIFDiff > 2) {
                    # Clear VIF difference - remove higher VIF
                    b00.removeVar <- ifelse(b00.var1VIF > b00.var2VIF, var1, var2)
                } else {
                    # Similar VIFs - remove lower adjusted R²
                    b00.removeVar <- ifelse(b00.var1AdjRSquared < b00.var2AdjRSquared, var1, var2)
                }
                
                b00.removedVars <- c(b00.removedVars, b00.removeVar)
                cat(sprintf("Removing %s\n", b00.removeVar))
            }
            
            b00.model3Vars <- b00.model2Vars[!b00.model2Vars %in% b00.removedVars]
            cat(sprintf("\nRemoved %d variables due to high correlations: %s\n", 
                        length(b00.removedVars), paste(b00.removedVars, collapse = ", ")))
        } else {
            b00.model3Vars <- b00.model2Vars
            cat("No high correlations found\n")
        }
    } else {
        b00.model3Vars <- b00.model2Vars
    }
    
    cat(sprintf("Variables remaining after correlation filtering: %d\n", length(b00.model3Vars)))
    
    if(length(b00.model3Vars) < 2) {
        cat("ERROR: Too few variables remaining after correlation filtering\n")
        return(NULL)
    }
    #-------------------------------------------------------------------------------------------------------------------
    #4: Then used leaps R PACKAGE to select MULTIVARIATE REGRESSION that PRODUCED HIGHEST R^2
    cat("\n--- Step 4: Best Subset Selection ---\n")
    tryCatch({
        b00.model3Data <- TSAndEVs2021[, c("thermalSensitivity", b00.model3Vars)]
        b00.bestSubset <- regsubsets(thermalSensitivity ~ ., data = b00.model3Data, 
                                    nbest = 1, nvmax = length(b00.model3Vars), method = "exhaustive")
        b00.bestSummary <- summary(b00.bestSubset)
        
        # Find model with highest adjusted R²
        b00.bestModelSize <- which.max(b00.bestSummary$adjr2)
        b00.bestPredictors <- names(which(b00.bestSummary$which[b00.bestModelSize, -1]))  # Exclude intercept
        
        cat(sprintf("Best subset selected %d variables (Adj R² = %.3f)\n", 
                    length(b00.bestPredictors), max(b00.bestSummary$adjr2)))
        cat("Variables in best model:", paste(b00.bestPredictors, collapse = ", "), "\n")
        
    }, error = function(e) {
        cat("ERROR in best subset selection:", e$message, "\n")
        return(NULL)
    })
    #-------------------------------------------------------------------------------------------------------------------
    #5: Then used lm.beta PACKAGE to EXTRACT STANDARDIZED BETA COEFFICIENT 
    # (estimates RELATIVE EFFECT of EACH EV on RV)
    cat("\n--- Step 5: Final Model Creation ---\n")
    tryCatch({
        b00.finalRegression <- paste("thermalSensitivity ~", paste(b00.bestPredictors, collapse = " + "))
        b00.model4 <- lm(as.formula(b00.finalRegression), data = TSAndEVs2021)
        b00.finalModel <- lm.beta(b00.model4)
        
        cat("Final model created successfully\n")
        cat("Model summary:\n")
        print(summary(b00.finalModel))
        
        return(list(
            model = b00.finalModel,
            variables = b00.bestPredictors,
            adj_r_squared = max(b00.bestSummary$adjr2),
            model_name = MLRModelName
        ))
        
    }, error = function(e) {
        cat("ERROR in final model creation:", e$message, "\n")
        return(NULL)
    })
}
#-------------------------------------------------------------------------------------------------------------------
#3 Function to check correlation coefficient direction & standardized coefficient direction - creating a stable MLR model
checkDirection <- function(MLRModel, corrMatrix, RV = "thermalSensitivity") {
    cat("\n=== CHECKING DIRECTION ===\n")
    
    # Extract standardized coefficients (exclude intercept)
    stdCoeffs <- coef(MLRModel)[-1]  # Remove intercept
    # Get simple correlations with thermal sensitivity
    correlations <- corrMatrix$r[RV, names(stdCoeffs)]
    # Check direction
    stdCoeffSign <- sign(stdCoeffs)
    corrSign <- sign(correlations)
    
    # Create results data frame object
    directionCheck <- data.frame(
        Variable = names(stdCoeffs),
        TSCorrelation = correlations,
        CorrelationDirection = ifelse(corrSign > 0, "Positive", "Negative"),
        StdCoefficient = stdCoeffs,
        StdCoefficientDirection = ifelse(stdCoeffSign > 0, "Positive", "Negative"),
        DirectionMatch = stdCoeffSign == corrSign,
        stringsAsFactors = FALSE
    )
    
    # Report mismatches
    mismatches <- directionCheck[!directionCheck$DirectionMatch, ]
    if(nrow(mismatches) > 0) {
        cat("WARNING: Direction mismatches found:\n")
        for(i in 1:nrow(mismatches)) {
            cat(sprintf("- %s: TS correlation %s, but standardized beta coefficient %s\n", 
                        mismatches$Variable[i], 
                        mismatches$CorrelationDirection[i], 
                        mismatches$StdCoefficientDirection[i]))
        }
    } else {
        cat("All variables show consistent directions\n")
    }
    
    return(directionCheck)
}
#-------------------------------------------------------------------------------------------------------------------
## Function to generate MLR Info Summary - similar to correlation info summary 
generateInfoSummary <- function(MLRModel, corrMatrix, directionCheck, MLRModelName) {
    cat("\n=== GENERATING MLR MODEL INFO SUMMARY ===\n")
    
    # Get model summary
    modelSum <- summary(MLRModel)
    
    # Extract coefficients (exclude intercept)
    coeffsTable <- modelSum$coefficients[-1, , drop = FALSE]  # Remove intercept row
    
    # Create comprehensive summary
    modelInfoSummary <- data.frame(
        Model = MLRModelName,
        Variable = rownames(coeffsTable),
        StdBetaCoefficient = coeffsTable[, "Estimate"],
        StdError = coeffsTable[, "Std. Error"],
        tValue = coeffsTable[, "t value"],
        pValue = coeffsTable[, "Pr(>|t|)"],
        Significance = ifelse(coeffsTable[, "Pr(>|t|)"] < 0.001, "***",
                             ifelse(coeffsTable[, "Pr(>|t|)"] < 0.01, "**",
                                   ifelse(coeffsTable[, "Pr(>|t|)"] < 0.05, "*",
                                         ifelse(coeffsTable[, "Pr(>|t|)"] < 0.1, ".", "")))),
        TSCorrelation = directionCheck$TSCorrelation,
        DirectionMatch = directionCheck$DirectionMatch,
        RSquared = modelSum$r.squared,
        AdjRSquared = modelSum$adj.r.squared,
        FStatistic = modelSum$fstatistic[1],
        F_pValue = pf(modelSum$fstatistic[1], modelSum$fstatistic[2], modelSum$fstatistic[3], lower.tail = FALSE),
        stringsAsFactors = FALSE
    )
    
    cat(sprintf("Model summary created for %s\n", MLRModelName))
    cat(sprintf("- %d variables included\n", nrow(modelInfoSummary)))
    cat(sprintf("- R² = %.3f, Adj R² = %.3f\n", unique(modelInfoSummary$RSquared), unique(modelInfoSummary$AdjRSquared)))
    cat(sprintf("- %d significant variables (p < 0.05)\n", sum(modelInfoSummary$pValue < 0.05)))
    cat(sprintf("- %d direction mismatches\n", sum(!modelInfoSummary$DirectionMatch)))
    
    return(modelInfoSummary)
}
#-------------------------------------------------------------------------------------------------------------------
## Function to compare MLR models - tracking
# Initialize comparison tracker
comparison_results <- data.frame()

addToComparison <- function(modelResult, directionCheck, MLRModelName, vifCutOff, corrCutOff) {
    if(is.null(modelResult)) {
        cat(sprintf("Cannot add %s to comparison - model generation failed\n", MLRModelName))
        return(comparison_results)
    }
    
    # Create comparison entry
    new_entry <- data.frame(
        ModelName = MLRModelName,
        VIFCutoff = vifCutOff,
        CorrCutoff = corrCutOff,
        NumVariables = length(modelResult$variables),
        AdjRSquared = modelResult$adj_r_squared,
        RSquared = summary(modelResult$model)$r.squared,
        FStatistic = summary(modelResult$model)$fstatistic[1],
        F_pValue = pf(summary(modelResult$model)$fstatistic[1], 
                     summary(modelResult$model)$fstatistic[2], 
                     summary(modelResult$model)$fstatistic[3], lower.tail = FALSE),
        DirectionMismatches = sum(!directionCheck$DirectionMatch),
        SignificantVars = sum(summary(modelResult$model)$coefficients[-1, "Pr(>|t|)"] < 0.05),
        Variables = paste(modelResult$variables, collapse = ", "),
        stringsAsFactors = FALSE
    )
    
    # Add to global comparison
    comparison_results <<- rbind(comparison_results, new_entry)
    
    cat(sprintf("Added %s to comparison tracker\n", MLRModelName))
    
    return(comparison_results)
}

showComparison <- function() {
    if(nrow(comparison_results) == 0) {
        cat("No models in comparison tracker yet.\n")
        return(NULL)
    }
    
    # Sort by adjusted R-squared (descending)
    sorted_comparison <- comparison_results[order(-comparison_results$AdjRSquared), ]
    
    cat("\n=== MODEL COMPARISON RESULTS ===\n")
    print(sorted_comparison[, c("ModelName", "AdjRSquared", "NumVariables", "DirectionMismatches", "SignificantVars")])
    
    return(sorted_comparison)
}
#-------------------------------------------------------------------------------------------------------------------
## Function to test multiple parameters for criteria cutoffs : VIF & correlation coefficient
testParameterCombinations <- function(vif_cutoffs = c(5, 10, 15, 20), 
                                     corr_cutoffs = c(0.6, 0.7, 0.8),
                                     variable_set = EVs2021) {
    
    cat("\n\n================= TESTING MANY PARAMETER COMBINATIONS =================\n")
    
    for(vif_cut in vif_cutoffs) {
        for(corr_cut in corr_cutoffs) {
            model_name <- sprintf("VIF%d_Corr%.1f", vif_cut, corr_cut)
            
            cat(sprintf("\n\nTesting VIF cutoff = %d, Correlation cutoff = %.1f\n", vif_cut, corr_cut))
            
            # Generate model
            model_result <- generateMLRModel(vif_cut, corr_cut, variable_set, model_name)
            
            if(!is.null(model_result)) {
                # Check direction consistency
                direction_check <- checkDirection(model_result$model, fullCorrMatrix)
                
                # Generate detailed summary
                detailed_summary <- generateInfoSummary(model_result$model, fullCorrMatrix, 
                                                       direction_check, model_name)
                
                # Add to comparison
                addToComparison(model_result, direction_check, model_name, vif_cut, corr_cut)
                
                # Save detailed results
                write_csv(detailed_summary, sprintf("results/2021/MLR/detailedSummary/%s_detailed_summary.csv", model_name))
                write_csv(direction_check, sprintf("results/2021/MLR/directionCheck/%s_direction_check.csv", model_name))
            }
        }
    }
    
    # Show final comparison
    final_comparison <- showComparison()
    
    # Save comparison table
    if(!is.null(final_comparison)) {
        write_csv(final_comparison, "results/2021/MLR/comparisonResults/model_comparison_results.csv")
    }
    
    return(final_comparison)
}
#-------------------------------------------------------------------------------------------------------------------
# Test single model - initial model 
cat("=== TESTING SINGLE MODEL ===\n")
noSolarModel2 <- generateMLRModel(vifCutOff = 20, corrCutOff = 0.6, 
                              includedEVs = EVs2021, MLRModelName = "noSolarModel2")

if(!is.null(noSolarModel2)) {
    # Check direction
    direction_results <- checkDirection(noSolarModel2$model, fullCorrMatrix)
    
    # Generate comprehensive summary
    model_summary <- generateInfoSummary(noSolarModel2$model, fullCorrMatrix, 
                                        direction_results, "noSolarModel2")
    
    # Add to comparison tracker
    addToComparison(noSolarModel2, direction_results, "noSolarModel2", 20, 0.6)
    
    # Save results
    write_csv(model_summary, "results/2021/MLR/infoSummary/noSolarModel2_summary.csv")
    write_csv(direction_results, "results/2021/MLR/directionCheck/noSolarModel2_direction_check.csv")
}
#-------------------------------------------------------------------------------------------------------------------
# Test multiple parameter combinations
cat("\n\n=== TESTING MULTIPLE COMBINATIONS ===\n")
all_results <- testParameterCombinations()

# Show final comparison of all models
cat("\n\n=== FINAL RESULTS ===\n")
showComparison()
#-------------------------------------------------------------------------------------------------------------------