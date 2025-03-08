
install.packages("quanteda")
install.packages("readtext")

# Load required libraries
library(quanteda)
library(readtext)
library(dplyr)
library(readr)

# Define the path to the text files
data_path <- "data/modified_data/finalized_data/"

# Get all CSV and TSV file names from the folder
file_list <- list.files(path = data_path, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Function to read CSV or TSV based on file extension
read_text_data <- function(file) {
  if (grepl("\\.csv$", file)) {
    read_csv(file, col_types = cols(.default = "c"))  # Read CSV
  } else {
    read_tsv(file, col_types = cols(.default = "c"))  # Read TSV
  }
}

# Read all files and combine them into one data frame
text_data <- bind_rows(lapply(file_list, read_text_data))

# Check column names and ensure 'speech' exists
if (!"speech" %in% colnames(text_data)) {
  stop("ERROR: Column 'speech' not found. Please verify the dataset structure.")
}

# Function to remove stage directions (e.g., "[Background noise]", "(Laughs)")
remove_stage_directions <- function(text) {
  gsub("\\[.*?\\]|\\(.*?\\)", "", text)  # Removes text in square or round brackets
}

# Clean the speech column
text_data$speech <- sapply(text_data$speech, remove_stage_directions)

# Create corpus
corpus_data <- corpus(text_data, text_field = "speech")

# Tokenize and process text
tokens_data <- tokens(corpus_data, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("es")) %>%
  tokens_wordstem(language = "es")  # Stemming in Spanish

# **Matrix 1: Concatenated Words (General Topic Classification)**
dfm_general <- dfm(tokens_data)

# **Matrix 2: Maintaining Structure (Turn-by-Turn Sentiment Analysis)**
dfm_turns <- dfm_group(dfm(tokens_data), groups = text_data$speaker)

# Save matrices for later analysis
saveRDS(dfm_general, "dfm_general.rds")
saveRDS(dfm_turns, "dfm_turns.rds")

# Display summary
summary(dfm_general)
summary(dfm_turns)
