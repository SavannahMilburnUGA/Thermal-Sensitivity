# Finding average DAYMET values for each site to merge w/ already derived TSAndEVs2021.csv - sorted w/ index
# added: daymet_: dayl, prcp, srad, swe (all 0), tmax, tmin, vp
# Now have 
library(dplyr)
library(readr)

# Paths for files 
main_csv_file <- "results/2021/SortedTSAndEVs2021.csv"
daymet_directory <- "daymet/results/filtered"           
output_file <- "results/2021/SortedDaymetTSAndEVs2021.csv"
# Function to iterate over 72 DAYMET files & find the average and merge data to SortedTSandEVs2021.csv
merge_daymet_with_main <- function(main_csv_path, daymet_folder, output_path = NULL) {
  
  # Read the main CSV file
  cat("Reading main CSV file...\n")
  main_df <- read_csv(main_csv_path, show_col_types = FALSE)
  cat("Main CSV has", nrow(main_df), "sites (rows)\n")
  
  # Get all DAYMET CSV files and sort them numerically so can merge correclty with SortedTSAndEVs2021.csv
  daymet_files <- list.files(daymet_folder, pattern = "\\.csv$", full.names = FALSE)
  # Extracting the number from filename
  extract_number <- function(filename) {
    # Extract number between "daymetfile" and ".csv"
    num_str <- sub("daymetfile(\\d+)\\.csv", "\\1", filename)
    return(as.numeric(num_str))
  }
  file_numbers <- sapply(daymet_files, extract_number)
  daymet_files <- daymet_files[order(file_numbers)]
  
  cat("Found", length(daymet_files), "DAYMET files\n")
  cat("First few files:", paste(head(daymet_files, 5), collapse = ", "), "\n")
  cat("Last few files:", paste(tail(daymet_files, 5), collapse = ", "), "\n")
  
  # Check if number of files matches number of sites - should be 72
  if (length(daymet_files) != nrow(main_df)) {
    warning("Number of DAYMET files (", length(daymet_files), 
            ") doesn't match number of sites (", nrow(main_df), ")")
  }
  
  # Initialize list to store average values for each DAYMET variable
  daymet_means <- list()
  
  # Iterate over each DAYMET file
  for (i in 1:length(daymet_files)) {
    filename <- daymet_files[i]
    
    cat("Processing file", i, "of", length(daymet_files), ":", filename, "\n")
    
    tryCatch({
      # Read DAYMET file
      file_path <- file.path(daymet_folder, filename)
      daymet_df <- read_csv(file_path, skip = 0, show_col_types = FALSE)
      
      # Check if required columns exist
      if (!("year" %in% colnames(daymet_df)) || !("yday" %in% colnames(daymet_df))) {
        cat("Warning:", filename, "missing 'year' or 'yday' columns. Skipping.\n")
        next
      }
      
      # Get all numeric columns except year and yday
      data_columns <- setdiff(names(daymet_df), c("year", "yday"))
      numeric_columns <- data_columns[sapply(daymet_df[data_columns], is.numeric)]
      # Calculate average values
      site_means <- daymet_df %>%
        summarise(across(all_of(numeric_columns), ~ mean(.x, na.rm = TRUE)))
      # Save average values for this site
      for (col in numeric_columns) {
        if (i == 1) {
          # Initialize the list for this column
          daymet_means[[col]] <- numeric(length(daymet_files))
          cat("  Initialized", col, "vector with length", length(daymet_files), "\n")
        }
        daymet_means[[col]][i] <- site_means[[col]]
      }
      
      cat("  Successfully processed", filename, "- calculated means for", 
          length(numeric_columns), "variables\n")
      
    }, error = function(e) {
      cat("Error processing", filename, ":", conditionMessage(e), "\n")
      
      # Fill with NA values for this site if we encounter an error
      if (length(daymet_means) > 0) {
        for (col in names(daymet_means)) {
          if (i <= length(daymet_means[[col]])) {
            daymet_means[[col]][i] <- NA
          }
        }
      }
    })
  }
  
  # Add DAYMET averages as new columns to SortedTSAndEVs2021.csv
  cat("\nAdding DAYMET means to main CSV...\n")
  
  for (col_name in names(daymet_means)) {
    # Add prefix to distinguish DAYMET variables
    new_col_name <- paste0("daymet_", col_name)
    main_df[[new_col_name]] <- daymet_means[[col_name]]
  }
  
  cat("Added", length(daymet_means), "new DAYMET mean columns\n")
  
  # Save the merged dataset
  if (is.null(output_path)) {
    output_path <- "results/2021/SortedDaymetTSAndEVs2021.csv"
  }
  write_csv(main_df, output_path)
  saveRDS(main_df, "results/2021/RDS/SortedDaymetTSAndEVs2021.RDS")
  cat("Saved merged dataset to:", output_path, "\n")
  
  # Print summary to debug
  cat("\nSummary:\n")
  cat("- Original columns:", ncol(main_df) - length(daymet_means), "\n")
  cat("- New DAYMET columns:", length(daymet_means), "\n")
  cat("- Total columns:", ncol(main_df), "\n")
  cat("- Total sites:", nrow(main_df), "\n")
  
  # Show what DAYMET variables were added
  daymet_col_names <- paste0("daymet_", names(daymet_means))
  cat("\nNew DAYMET columns added:\n")
  for (col in daymet_col_names) {
    cat("-", col, "\n")
  }
  
  return(main_df)
}

# Run the merge - added: daymet_: dayl, prcp, srad, swe (all 0), tmax, tmin, vp
merged_data <- merge_daymet_with_main(main_csv_file, daymet_directory, output_file)
