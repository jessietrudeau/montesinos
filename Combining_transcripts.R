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












# Combine all valid dataframes
combined_df <- bind_rows(all_data) %>%
  # Add a unique line‑ID column
  mutate(line_id = row_number()) %>%
  # Reorder so that line_id is 1st, text is 2nd
  select(line_id, text, everything())











library(dplyr)
library(readr)
library(tools)

# Set your directory containing the files
data_dir <- "data/modified_data/finalized_data"  

# List all .csv and .tsv files
all_files <- list.files(data_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Initialize list for storing dataframes
all_data <- list()

# Loop through each file
for (file_path in all_files) {
  file_name <- file_path_sans_ext(basename(file_path))
  delim <- ifelse(grepl("\\.tsv$", file_path), "\t", ",")
  
  # Try reading and processing the file
  df <- tryCatch({
    read_delim(file_path, delim = delim, show_col_types = FALSE) %>%
      # remove any BACKGROUND rows
      filter(is.na(speaker_std) | speaker_std != "BACKGROUND") %>%
      # remove old speaker column if present
      select(-any_of("speaker")) %>%     
      # add file identifier
      mutate(file_id = file_name)
  }, error = function(e) {
    message(sprintf("Skipping file due to error: %s\n%s", file_name, e$message))
    NULL
  })
  
  # Append successful reads
  if (!is.null(df)) {
    all_data[[length(all_data) + 1]] <- df
  }
}

# Combine all valid dataframes
combined_df <- bind_rows(all_data)

# Add a unique row ID column and make it first
combined_df <- combined_df %>%
  mutate(row_id = row_number()) %>%
  select(row_id, everything())

# Optional: write to CSV
write_csv(combined_df, "all_transcripts_cleaned.csv")

# Preview
print(head(combined_df))












# --- Version A: the 4‑column slice ---
four_col_df <- combined_df %>%
  select(
    row_id,    # transcript identifier
    speech,    # your new row identifier
    speaker_std,     # e.g. “text” or whatever your third key column is
    file_id      # e.g. “timestamp” or another measure
  )

# write it out
write_csv(four_col_df, "transcript_fourcols.csv")


# --- Version B: the “joinable” slice for left_join() ---
# only keep the keys you’ll join on (plus any small lookup fields you need)
join_keys_df <- four_col_df %>%
  select(
    file_id,
    row_id
  )

# write it out
write_csv(join_keys_df, "transcript_keys.csv")


# --- (Later) joining back to the full dataset by transcript & line ---
# full_df is your master data that already has file_id + line_id
analysis_df <- four_col_df %>%
  left_join(join_keys_df, by = c("file_id","row_id"))














# how many rows total?
N <- nrow(combined_df)

# batch 1: rows 1 through 20 000 (or up to N if N < 20 000)
batch1 <- combined_df %>%
  slice(1:min(20000, N))

# batch 2: rows 20 001 through N (empty if N ≤ 20 000)
batch2 <- combined_df %>%
  slice((min(20000, N) + 1):N)

# (optional) write them out
write_csv(batch1, "batch1.csv")
write_csv(batch2, "batch2.csv")




