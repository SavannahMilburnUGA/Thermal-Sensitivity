## Clean house & remove saved files 
# Remove all objects in workspace
rm(list = ls())
# Close old plots
while (!is.null(dev.list())) dev.off()

# Make huge spreadsheet to join EVs from Hydroshare
# Run correlation analysis

library(tidyverse)

# Load thermalSensitivities2021 RDS
thermalSensitivities2021 <- readRDS("thermalSensitivities2021.RDS")
# Check
nrow(thermalSensitivities2021) # 73 sites 

# Load CRB EVs - from Michael
library(readxl)
fileEVs2021 = "C:/Users/savan/OneDrive/Desktop/NSF PSU REU/Thermal Sensitivity/Thermal-Sensitivity/EVs/sites_attributes_2021.xlsx"
# Check for file exists
file.exists(fileEVs2021)
print(fileEVs2021)
ClackEVs2021 <- read_excel(fileEVs2021)
# Check
head(ClackEVs2021)

# Rename 4 USGS sites so can join ClackEVs2021 + thermalSensitivities2021
ClackEVs2021 <- ClackEVs2021 %>%
  mutate(Filename = case_when(
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - CARTER BRIDGE" ~ "USGS 1",
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - ESTACADA" ~ "USGS 2", 
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - OREGON CITY" ~ "USGS 3",
    Filename == "CLACKAMAS.TEMPERATURES.USGS.2021 - FISH CREEK" ~ "USGS 4",
    TRUE ~ Filename
  ))
# Check
View(ClackEVs2021) # USGS 1-4

# Remove 7 unneeded sites from ClackEVs2021 to make 73 sites 
remove <- c("10598088", "20733169", "USGS 3", "10361309", "10931479", "11007911", "11007937", "20539817")
ClackEVs2021 <- ClackEVs2021 %>%
  filter(!Filename %in% remove)
# Check
nrow(ClackEVs2021) # Should be 73

# Join thermal sensitivities + EVs - huge spreadsheet for correlation analysis
TSAndEVs <- thermalSensitivities2021 %>%
  left_join(ClackEVs2021, by = c("site" = "Filename")) %>%
  filter(!is.na(thermalSensitivity))
# Check 
nrow(TSAndEVs)

# Define EVs/landscape variables from Michael 
# Separated based on scales too: upstream, catchment, buffer
EVs <- c(
  # NHDPlus v2 + Krochta + Chang
  "SLOPE", "Solar", "Elev", "BFI",

  # Upstream 
  "h2oDevelop", "h2oLakesPe", "h2oAgricul", "h2oBurnPer", "h2oRdDens", "h2oHiCascP", 
  "h2oWetland", "h2oVegCov", "h2oVegHt", "Forest21", "Shrub21", "h2oKm2",

  # Catchment 
   "BurnRCA", "AgricultRC", "WetlandsRC", "LakesRCA", "HiCascRCA", 
  "DevelopRCA", "RoadsRCA", "VegCover", "VegHeight_",

  # Buffer
  "DevelopBuf", "AgBuf", "BurnBuf", "WetlandBuf", 
  "LakesBuf", "HiCascBuf", "RoadsBuf", "VegHtBuf", "VegCovBuf",
  
  # Climate - calculated from PRISM
  "MeanMaxAir", "MaxAir_C", "Precip_mm", "SumPrecip", "MeanAirJJA", "WetPrecip"
)

# Check which variables exist in joined TSAndEVs dataset
available_vars <- intersect(EVs, names(TSAndEVs))
missing_vars <- setdiff(EVs, names(TSAndEVs))
cat("\nVariable check:\n")
cat("Available landscape variables:", length(available_vars), "\n")
cat("Missing variables:", length(missing_vars), "\n")
if(length(missing_vars) > 0) {
  cat("Missing:", paste(missing_vars, collapse = ", "), "\n")
}

# Load required libraries
library(corrplot)
library(Hmisc)
library(gridExtra)
library(psych)

# Prepare correlation dataset
corrData <- TSAndEVs %>%
  select(thermalSensitivity, all_of(available_vars))

# Check data completeness
cat("\nData quality for correlation analysis:\n")
cat("Number of variables:", ncol(corrData), "\n")
cat("Number of sites:", nrow(corrData), "\n")

# Check for missing values for each variable
missing_summary <- corrData %>%
  summarise_all(~sum(is.na(.))) %>%
  gather(variable, missing_count) %>%
  arrange(desc(missing_count))
cat("\nMissing values per variable:\n")
print(missing_summary)

# Remove variables with too many missing values (>20% missing)
threshold_missing <- 0.2 * nrow(corrData)
vars_to_keep <- missing_summary %>%
  filter(missing_count <= threshold_missing) %>%
  pull(variable)

# Remove variables w/ too many missing values ( > 20%)
# Remove sites with any missing values
corrDataClean <- corrData %>%
  select(all_of(vars_to_keep)) %>%
  drop_na()  # Remove rows with any missing values

cat("\nAfter cleaning:\n")
cat("Variables retained:", ncol(corrDataClean), "\n")
cat("Sites with complete data:", nrow(corrDataClean), "\n")

# Save cleaned dataset for future analysis
write.csv(corrDataClean, "cleaned_thermal_sensitivity_dataset.csv", row.names = FALSE)
cat("Saved cleaned dataset to CSV file\n")

# Save data quality summary
data_quality_summary <- data.frame(
  metric = c("Original sites", "Sites after removing unwanted", "Sites with complete data", 
             "Original variables", "Variables retained", "Variables analyzed"),
  count = c(nrow(thermalSensitivities2021), nrow(TSAndEVs), nrow(corrDataClean),
            length(EVs), ncol(corrDataClean), ncol(corrDataClean)-1)
)
write.csv(data_quality_summary, "data_quality_summary.csv", row.names = FALSE)
cat("Saved data quality summary to CSV file\n")

cat("\n=== VERIFYING SPEARMAN CORRELATION APPROACH ===\n")
# Check distributions (Spearman is good for non-normal data)
cat("Checking thermal sensitivity distribution:\n")
print(summary(corrDataClean$thermalSensitivity))

# Shapiro-Wilk test for normality (if p < 0.05, data is non-normal so Spearman good)
shapiroTest <- shapiro.test(corrDataClean$thermalSensitivity)
cat("Shapiro-Wilk test for thermal sensitivity normality:\n")
cat("W =", round(shapiroTest$statistic, 4), ", p-value =", 
    format(shapiroTest$p.value, scientific = TRUE), "\n")

if(shapiroTest$p.value < 0.05) {
  cat("Data is non-normal so Spearman correlation is appropriate\n")
} else {
  cat("Data appears normal so Both Pearson and Spearman would work\n")
}

cat("\n=== SPEARMAN CORRELATION ANALYSIS ===\n")
# Calculate Spearman rank correlations b/w all variables
# Gets correlation coefficients & P-values
# Method 1: Using Hmisc::rcorr (recommended - gives p-values)
corrMatrix <- rcorr(as.matrix(corrDataClean), type = "spearman")

# Extract results
corrCoef <- corrMatrix$r
pValues <- corrMatrix$P

# Save full correlation matrix as CSV (NOW that they exist)
write.csv(corrCoef, "full_correlation_matrix.csv", row.names = TRUE)
cat("Saved full correlation matrix to CSV file\n")

# Save p-values matrix as CSV  
write.csv(pValues, "correlation_pvalues_matrix.csv", row.names = TRUE)
cat("Saved p-values matrix to CSV file\n")

# Focus on thermal sensitivity correlations
TSCorrCoeff <- corrCoef["thermalSensitivity", ]
TSPVals <- pValues["thermalSensitivity", ]

# Extracts only correlations b/w thermal sensitivity & landscape variables
# Strongest correlations first
# Remove self-correlation and combine with p-values
thermal_results <- data.frame(
  variable = names(TSCorrCoeff),
  correlation = TSCorrCoeff,
  p_value = TSPVals,
  stringsAsFactors = FALSE
) %>%
  filter(variable != "thermalSensitivity") %>%
  mutate(
    abs_correlation = abs(correlation),
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**", 
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(desc(abs_correlation))

cat("Thermal sensitivity correlations (strongest first):\n")
print(thermal_results, row.names = FALSE)

# Save thermal sensitivity correlation results as CSV
write.csv(thermal_results, "thermal_sensitivity_correlations.csv", row.names = FALSE)
cat("Saved thermal sensitivity correlations to CSV file\n")

# VERIFICATION METHODS
# ===================

cat("\n=== VERIFICATION METHODS ===\n")

# Method A: Compare with base R cor() function
cat("Verification A: Compare with base R cor() function\n")
base_r_corr <- cor(corrDataClean, method = "spearman", use = "complete.obs")
thermal_base <- base_r_corr["thermalSensitivity", ]

# Check if results match
matches <- all.equal(TSCorrCoeff, thermal_base)
cat("Hmisc and base R results match:", is.logical(matches) && matches, "\n")

# Method B: Manual calculation for one variable (as example)
cat("\nVerification B: Manual Spearman calculation example\n")
# Pick first landscape variable for manual verification
test_var <- thermal_results$variable[1]
cat("Testing variable:", test_var, "\n")

# Manual Spearman calculation
x_ranks <- rank(corrDataClean$thermalSensitivity)
y_ranks <- rank(corrDataClean[[test_var]])
manual_spearman <- cor(x_ranks, y_ranks)

auto_spearman <- TSCorrCoeff[test_var]
cat("Manual Spearman:", round(manual_spearman, 6), "\n")
cat("Automatic Spearman:", round(auto_spearman, 6), "\n")
cat("Match:", round(manual_spearman, 6) == round(auto_spearman, 6), "\n")

# Method C: Check with psych package
psych_result <- corr.test(corrDataClean, method = "spearman")
psych_thermal <- psych_result$r["thermalSensitivity", ]
cat("\nVerification C: psych package results match:", 
    all.equal(TSCorrCoeff, psych_thermal), "\n")

# VISUALIZATION
# =============

cat("\n=== CREATING VISUALIZATIONS ===\n")

# Simple fix - use matrices as they are since correlation worked perfectly
corrCoef_clean <- corrCoef
pValues_clean <- pValues

cat("Correlation matrix dimensions:", dim(corrCoef_clean), "\n")

# Visualization 1: correlation heatmap
png("spearman_correlation_matrix.png", width = 14, height = 12, units = "in", res = 300)
corrplot(corrCoef_clean,
         method = "color",
         type = "upper",
         order = "hclust", 
         tl.cex = 0.6,
         tl.col = "black",
         tl.srt = 45,
         p.mat = pValues_clean,
         sig.level = 0.05,
         insig = "blank",
         title = "Spearman Correlations (only p < 0.05 shown)",
         mar = c(0,0,2,0),
         col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()
cat("Created correlation matrix plot\n")

# Visualization 2: Thermal sensitivity specific correlations
strong_corr <- thermal_results %>%
  filter(abs_correlation > 0.3) %>%
  arrange(correlation)

if(nrow(strong_corr) > 0) {
  png("thermal_sensitivity_correlations.png", width = 12, height = 8, units = "in", res = 300)
  par(mar = c(12, 4, 4, 2))
  barplot(strong_corr$correlation,
          names.arg = strong_corr$variable,
          main = "Thermal Sensitivity: Strong Correlations (|r| > 0.3)",
          las = 2,
          col = ifelse(strong_corr$correlation > 0, "red3", "blue3"),
          ylab = "Spearman correlation coefficient",
          cex.names = 0.8)
  abline(h = 0, lty = 2)
  
  # Add significance stars
  text(x = 1:nrow(strong_corr), 
       y = strong_corr$correlation + 0.02 * sign(strong_corr$correlation),
       labels = strong_corr$significance,
       cex = 1.2)
  dev.off()
  
  cat("Created plot with", nrow(strong_corr), "strong correlations\n")
} else {
  cat("No correlations > |0.3| found for separate plot\n")
}

# Visualization 3: Scatterplots of strongest correlations
top_3_vars <- head(thermal_results, 3)$variable

if(length(top_3_vars) >= 2) {
  png("correlation_scatterplots.png", width = 15, height = 5, units = "in", res = 300)
  par(mfrow = c(1, 3))
  
  for(i in 1:min(3, length(top_3_vars))) {
    var_name <- top_3_vars[i]
    corr_val <- thermal_results[thermal_results$variable == var_name, "correlation"]
    p_val <- thermal_results[thermal_results$variable == var_name, "p_value"]
    
    plot(corrDataClean[[var_name]],
         corrDataClean$thermalSensitivity,
         xlab = var_name,
         ylab = "Thermal Sensitivity", 
         main = paste0("r = ", round(corr_val, 3), 
                      ", p = ", format(p_val, digits = 3)),
         pch = 16, col = "blue")
    
    # Add trend line
    abline(lm(corrDataClean$thermalSensitivity ~ 
              corrDataClean[[var_name]]), col = "red", lwd = 2)
  }
  
  par(mfrow = c(1, 1))
  dev.off()
  cat("Created scatterplots of top 3 correlations\n")
}

# Visualization 4: Distribution and boxplots
png("thermal_sensitivity_distribution.png", width = 10, height = 6, units = "in", res = 300)
par(mfrow = c(1, 2))

# Histogram
hist(corrDataClean$thermalSensitivity, 
     main = "Distribution of Thermal Sensitivity",
     xlab = "Thermal Sensitivity",
     col = "lightblue",
     breaks = 15)

# Boxplot by quartiles of strongest predictor
strongest_var <- thermal_results$variable[1]
quartiles <- cut(corrDataClean[[strongest_var]], 
                breaks = quantile(corrDataClean[[strongest_var]], na.rm = TRUE),
                include.lowest = TRUE,
                labels = c("Q1", "Q2", "Q3", "Q4"))

boxplot(corrDataClean$thermalSensitivity ~ quartiles,
        main = paste("Thermal Sensitivity by", strongest_var, "Quartiles"),
        xlab = paste(strongest_var, "Quartiles"),
        ylab = "Thermal Sensitivity",
        col = c("lightcoral", "lightblue", "lightgreen", "lightyellow"))

par(mfrow = c(1, 1))
dev.off()
cat("Created thermal sensitivity distribution plots\n")

# SUMMARY OUTPUT
# ==============

cat("\n=== SUMMARY ===\n")
cat("Dataset: ", nrow(corrDataClean), " sites, ",
    ncol(corrDataClean)-1, " landscape variables\n")

significant_corr <- thermal_results %>% filter(p_value < 0.05)
cat("Significant correlations (p < 0.05):", nrow(significant_corr), "\n")

strong_significant <- thermal_results %>% filter(p_value < 0.05 & abs_correlation > 0.3)
cat("Strong significant correlations (|r| > 0.3, p < 0.05):", nrow(strong_significant), "\n")

if(nrow(strong_significant) > 0) {
  cat("\nStrongest significant relationships:\n")
  print(strong_significant[1:min(5, nrow(strong_significant)), 
                          c("variable", "correlation", "p_value", "significance")], 
        row.names = FALSE)
  
  # Save strong significant correlations
  write.csv(strong_significant, "strong_significant_correlations.csv", row.names = FALSE)
  cat("Saved strong significant correlations to CSV file\n")
}

# Create comprehensive summary table
summary_stats <- data.frame(
  analysis_step = c("Total correlations tested", "Significant correlations (p < 0.05)", 
                   "Strong correlations (|r| > 0.3)", "Strong significant correlations",
                   "Very strong correlations (|r| > 0.5)", "Strongest positive correlation",
                   "Strongest negative correlation"),
  count_or_value = c(nrow(thermal_results), nrow(significant_corr), 
                    nrow(thermal_results[thermal_results$abs_correlation > 0.3,]),
                    nrow(strong_significant),
                    nrow(thermal_results[thermal_results$abs_correlation > 0.5,]),
                    paste(thermal_results[which.max(thermal_results$correlation), "variable"], 
                          "r =", round(max(thermal_results$correlation), 3)),
                    paste(thermal_results[which.min(thermal_results$correlation), "variable"],
                          "r =", round(min(thermal_results$correlation), 3)))
)
write.csv(summary_stats, "correlation_analysis_summary.csv", row.names = FALSE)
cat("Saved correlation analysis summary to CSV file\n")

cat("\nGenerated files:\n")
cat("=== CSV FILES ===\n")
cat("- thermal_sensitivity_correlations.csv: All correlations with thermal sensitivity\n")
cat("- cleaned_thermal_sensitivity_dataset.csv: Final dataset used for analysis\n") 
cat("- full_correlation_matrix.csv: Complete correlation matrix (all variables)\n")
cat("- correlation_pvalues_matrix.csv: P-values for all correlations\n")
cat("- data_quality_summary.csv: Data cleaning and sample size summary\n")
cat("- strong_significant_correlations.csv: Key relationships (|r| > 0.3, p < 0.05)\n")
cat("- correlation_analysis_summary.csv: Overall analysis statistics\n")

cat("\n=== PNG FILES ===\n")
cat("- spearman_correlation_matrix.png: Full correlation heatmap\n")
if(nrow(strong_corr) > 0) {
  cat("- thermal_sensitivity_correlations.png: Strong correlations barplot\n")
}
if(length(top_3_vars) >= 2) {
  cat("- correlation_scatterplots.png: Scatterplots of top 3 relationships\n")
}
cat("- thermal_sensitivity_distribution.png: Distribution and boxplots\n")

cat("\n=== VERIFICATION COMPLETE ===\n")
cat("Spearman correlation analysis verified using multiple methods\n")