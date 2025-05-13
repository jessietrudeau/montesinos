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
combined_df <- combined_df %>% mutate(id = row_number()) %>% select(id, everything())
#combined_df <- combined_df %>% rename(text = speech)
colnames(combined_df)[2] <- text

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

# Add a unique row ID column
combined_df <- combined_df %>% mutate(id = row_number()) %>% select(id, everything())
#combined_df <- combined_df %>% rename(text = speech)
colnames(combined_df)[2] <- "text"

# Optional: write to CSV
write_csv(combined_df, "all_transcripts_cleaned.csv")

# Preview
print(head(combined_df))




transcript_notes_df<-read_csv("data/Transcript inventory.csv")










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
    row_id,
    speech
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
write_csv(batch1, "data/babel_batches/batch1.csv")
write_csv(batch2, "data/babel_batches/batch2.csv")


sum(batch2$speaker_std == "BACKGROUND")

























library(dplyr)
library(readr)
library(tools)
library(tidyr)
library(stringr)

# 1) Load & reshape your inventory of topics
inventory <- read_csv("data/Transcript inventory.csv", col_types = cols())

# pivot longer so each topic flag becomes a row, then collapse back into a
# comma-separated 'topics' string per transcript n
topics_lookup <- inventory %>%
  pivot_longer(
    cols = starts_with("topic_"),
    names_to  = "topic",
    values_to = "flag"
  ) %>%
  filter(!is.na(flag) & flag != 0) %>%      # keep only flagged topics
  mutate(topic = str_remove(topic, "^topic_")) %>%
  group_by(n) %>%
  summarize(
    topics = paste(topic, collapse = ", "),
    .groups = "drop"
  )

# Read all your per-transcript files and tag them with file_id
data_dir <- "data/modified_data/finalized_data"
all_files <- list.files(data_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)

all_data <- list()
for (file_path in all_files) {
  file_name <- file_path_sans_ext(basename(file_path))
  
  df <- tryCatch(
    read_delim(file_path, delim = ifelse(grepl("\\.tsv$", file_path), "\t", ","), show_col_types = FALSE) %>%
      filter(is.na(speaker_std) | speaker_std != "BACKGROUND") %>%
      select(-any_of("speaker")) %>%
      mutate(
        file_id = as.integer(file_name)       # ensure numeric for join
      ) %>%
      # join in the topics for this file_id
      left_join(topics_lookup, by = c("file_id" = "n")),
    error = function(e) {
      message("Skipping ", file_name, ": ", e$message)
      NULL
    }
  )
  
  if (!is.null(df)) all_data[[length(all_data) + 1]] <- df
}

# 4) bind, add row id, rename and write out
combined_df <- bind_rows(all_data) %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  rename(text = speech)

write_csv(combined_df, "data/babel_df.csv")

# Preview
print(head(combined_df))


unique(combined_df$topics)

unique(combined_df$file_id)

sum(is.na(combined_df$topics))




write_csv(transcript_notes_df, "test.csv")











no_topic_count <- inventory %>%
  filter(if_all(starts_with("topic_"), ~ is.na(.))) %>%
  tally()

print(no_topic_count)



missing_topics_ids <- combined_df %>%
  filter(is.na(topics) | topics == "") %>%   # catch NA or empty-string
  distinct(file_id)

print(arrange(desc(missing_topics_ids$file_id)))

order(missing_topics_ids)



library(dplyr)

# Sort missing_topics_ids by file_id in descending order
missing_topics_ids %>%
  arrange(file_id)  


