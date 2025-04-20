# Load necessary library
library(dplyr)

# Set your directory containing the files
data_dir <- "data/modified_data/finalized_data"  

# List all .csv and .tsv files
all_files <- list.files(data_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Initialize list for storing dataframes
all_data <- list()

# Loop through each file
for (file_path in all_files) {
  file_name <- tools::file_path_sans_ext(basename(file_path))
  delim <- ifelse(grepl("\\.tsv$", file_path), "\t", ",")
  
  # Try reading and processing the file
  df <- tryCatch({
    read_delim(file_path, delim = delim, show_col_types = FALSE) %>%
      select(-any_of("speaker")) %>%     # Remove speaker if present
      mutate(file_id = file_name)        # Add file ID
  }, error = function(e) {
    message(sprintf("Skipping file due to error: %s\n%s", file_name, e$message))
    NULL  # Return NULL if file fails to load
  })
  
  # Append successful reads
  if (!is.null(df)) {
    all_data[[length(all_data) + 1]] <- df
  }
}

# Combine all valid dataframes
combined_df <- bind_rows(all_data)

# Add a unique row ID column
combined_df <- combined_df %>% mutate(row_id = row_number()) %>% select(row_id, everything())

# Optional: write to CSV
write_csv(combined_df, "all_transcipts_cleaned.csv")

# Preview
print(head(combined_df))




