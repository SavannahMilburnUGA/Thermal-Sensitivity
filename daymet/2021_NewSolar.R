# Finding the new DAYMET solar radiation values for each site
library(dplyr)
library(readr)

# DAYMET CSV Filter Script for R
# Filter 72 DAYMET CSV files to include only July 1 - August 31, 2021

# Function to filter DAYMET files
filter_daymet_files <- function(input_directory, output_directory = NULL) {
  # Correcting to 7/1/2021 to 8/31/2021 date range
  year2021 <- 2021
  july1 <- 182
  aug31 <- 243
  
  # Set output directory
  if (is.null(output_directory)) {
    output_directory <- input_directory
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  
  # Get all CSV files in the directory
  daymetOutputFiles <- list.files(input_directory, pattern = "\\.csv$", full.names = FALSE)
  
  cat("Found", length(daymetOutputFiles), "CSV files to process\n")
  
  processed_count <- 0
  
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
      
      # Filter for 2021 and the specific day range
      filtered_df <- df %>%
        filter(year == year2021 & 
               yday >= july1 & 
               yday <= aug31)
      
      # Keep the same filename (this was missing in your code)
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

# Since you're running this script from the daymet folder:
input_dir <- "daymet/results"           # The subfolder containing your 72 CSV files
output_dir <- "daymet/results/filtered"   # The subfolder you created for filtered files

# Run the filtering - this will save filtered files in results/filter/
filter_daymet_files(input_dir, output_dir)
