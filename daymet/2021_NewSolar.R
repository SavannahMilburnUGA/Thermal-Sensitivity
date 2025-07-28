# Filter saved DAYMET csv files to target date range: 7/1/2021 - 8/31/2021
library(dplyr)
library(readr)

# Directories to save to 
input_dir <- "daymet/results"          
output_dir <- "daymet/results/filtered"  

# Function to filter DAYMET files to date raneg
filter_daymet_files <- function(input_directory, output_directory = NULL) {
  year2021 <- 2021
  july1 <- 182
  aug31 <- 243
  
  # Set output directory
  if (is.null(output_directory)) {
    output_directory <- input_directory
  }
  
  # Get all CSV files outputted from DAYMET
  daymetOutputFiles <- list.files(input_directory, pattern = "\\.csv$", full.names = FALSE)
  # Should be 72
  cat("Found", length(daymetOutputFiles), "CSV files to process\n")
  
  processed_count <- 0
  # Iterate over 72 DAYMET files to filter to proper date range
  for (filename in daymetOutputFiles) {
    tryCatch({
      # Read the CSV file
      file_path <- file.path(input_directory, filename)
      df <- read_csv(file_path, skip = 6, show_col_types = FALSE)
      
      # Check if required columns exist
      if (!("year" %in% colnames(df)) || !("yday" %in% colnames(df))) {
        cat("Warning:", filename, "missing 'year' or 'yday' columns. Skipping.\n")
        next
      }
      
      # Filter to 7/1/2021 - 8/31/2021
      filtered_df <- df %>%
        filter(year == year2021 & 
               yday >= july1 & 
               yday <= aug31)
      output_filename <- filename
      
      # Save the filtered data
      output_path <- file.path(output_directory, output_filename)
      write_csv(filtered_df, output_path)
      
      cat("Processed", filename, ":", nrow(filtered_df), "rows of July-August 2021 data saved\n")
      processed_count <- processed_count + 1
      
    }, error = function(e) {
      cat("Error processing", filename, ":", conditionMessage(e), "\n")
    })
  }
  
  cat("\nCompleted! Processed", processed_count, "out of", length(daymetOutputFiles), "files\n")
  return(processed_count)
} 

# Filter to target date range: 7/1 to 8/31 2021
filter_daymet_files(input_dir, output_dir)