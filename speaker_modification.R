library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Directory with transcripts
transcript_dir <- "data/modified_data/finalized_data"
transcript_files <- list.files(transcript_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Target speaker_std values
target_speaker_std <- c(
  "ALVA", "LEWIS", "BURNET", "GARCIA", "DE LOPEZ", "SMITH", "VILLAREAL", "AYBAR", "LOPEZ", "MALEA",
  "MANUEL LOPEZ", "GONZALES", "AMERICANO", "COSTA", "MACCAFFREY", "SAUCEDO SANCHEZ", "HERMOZA",
  "IBÁRCENA", "BELLO VASQUEZ", "PANDOLFI", "PERCOVICH", "JETT", "ARANCIBIA", "LUPIS", "BURNEI",
  "ALEX KOURI", "FRANCISCO KOURI", "MONCAYO", "GUILLEN", "ALBARRACÍN", "CANO ANGULO", "SALAZAR",
  "CESAR CANO", "RICKETTS", "ROZAS", "REPRESENTANTE ESTADOS UNIDOS", "GENERAL DELGADO", "PALERMO",
  "CHIRINOS", "VERA ABAD", "VIDAL", "MOROTE", "PANTOJA", "DOUFOUR", "ALBERTO KOURI", "BOLONA",
  "SCHüTZ", "PEREZ", "CENEPO"
)

# Helper to read CSV/TSV
read_transcript <- function(fp) {
  if (str_detect(fp, "\\.csv$")) read_csv(fp, col_types = cols()) else read_tsv(fp, col_types = cols())
}

# Extract all unique (speaker, speaker_std) pairs that match the target list
unique_speaker_pairs <- map_df(transcript_files, ~ {
  df <- read_transcript(.x)
  if (all(c("speaker", "speaker_std") %in% names(df))) {
    df %>%
      filter(speaker_std %in% target_speaker_std) %>%
      select(speaker, speaker_std) %>%
      distinct()
  } else {
    tibble(speaker = character(0), speaker_std = character(0))
  }
}) %>%
  distinct() %>%  # Global deduplication across all files
  arrange(speaker_std, speaker)

# View result
print(unique_speaker_pairs)

write_csv(unique_speaker_pairs, "missing_speakers.csv")

















unique(read_csv("data/modified_data/finalized_data/")$speaker_std)

# Target speaker_std
target <- "EMBAJADORA"

# Check each file
transcripts_with_target <- map_chr(transcript_files, function(fp) {
  df <- if (str_detect(fp, "\\.csv$")) read_csv(fp, col_types = cols()) else read_tsv(fp, col_types = cols())
  
  if ("speaker_std" %in% names(df) && target %in% df$speaker_std) {
    return(basename(fp))
  } else {
    return(NA_character_)
  }
}) %>%
  na.omit()

# Output matching transcripts
print(transcripts_with_target)




















# Load libraries
library(tidyverse)

# Path to finalized transcripts
transcript_path <- "data/modified_data/finalized_data/"
transcript_files <- list.files(transcript_path, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Regex pattern for currency (case-insensitive)
currency_pattern <- regex("\\bsol(?:es)?\\b|\\bdolares\\b|\\bdollars\\b|\\$|\\bPEN\\b", ignore_case = TRUE)

# Initialize list of matching transcripts
currency_transcripts <- c()

# Loop through all files
for (file in transcript_files) {
  try({
    # Use appropriate reader
    df <- if (str_detect(file, "\\.csv$")) {
      read_csv(file, col_types = cols())
    } else {
      read_tsv(file, col_types = cols())
    }
    
    # If the 'speech' column exists, check for currency mentions
    if ("speech" %in% colnames(df)) {
      if (any(str_detect(df$speech, currency_pattern))) {
        currency_transcripts <- c(currency_transcripts, basename(file))
      }
    }
  }, silent = TRUE)
}

# Print the names of matching files
print(currency_transcripts)











# Load libraries
library(tidyverse)

# Path to finalized transcripts
transcript_path <- "data/modified_data/finalized_data/"
transcript_files <- list.files(transcript_path, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Define regex pattern for currency (case-insensitive)
currency_pattern <- regex("\\bsol(?:es)?\\b|\\bdolares\\b|\\bdollars\\b|\\$|\\bPEN\\b", ignore_case = TRUE)

# Initialize list to collect all matching rows across transcripts
all_matches <- list()

# Loop through all transcript files
for (file in transcript_files) {
  try({
    # Read CSV or TSV accordingly
    df <- if (str_detect(file, "\\.csv$")) {
      read_csv(file, col_types = cols())
    } else {
      read_tsv(file, col_types = cols())
    }
    
    # If 'speech' column exists
    if ("speech" %in% colnames(df)) {
      # Filter for rows where speech matches the currency pattern
      matches <- df %>% filter(str_detect(speech, currency_pattern))
      
      # If matches found, add filename column and store
      if (nrow(matches) > 0) {
        matches <- matches %>% mutate(source_file = basename(file))
        all_matches[[length(all_matches) + 1]] <- matches
      }
    }
  }, silent = TRUE)
}

# Combine all matched rows into one dataframe
currency_mentions_df <- bind_rows(all_matches)

# Print the results
print(currency_mentions_df)

write_csv(currency_mentions_df, "transaction_tracking.csv")

































library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Define directory path
dir_path <- "data/modified_data/finalized_data"

# List all .csv and .tsv files
transcript_files <- list.files(path = dir_path, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Initialize empty vector to store speaker names
all_speakers <- c()

# Loop over each file
for (file in transcript_files) {
  # Read the file depending on its extension
  file_data <- tryCatch({
    if (str_detect(file, "\\.csv$")) {
      read_csv(file, col_types = cols())
    } else {
      read_tsv(file, col_types = cols())
    }
  }, error = function(e) {
    message(paste("Error reading file:", file))
    return(NULL)
  })
  
  # If speaker_std column exists, extract it
  if (!is.null(file_data) && "speaker_std" %in% colnames(file_data)) {
    speakers <- unique(file_data$speaker_std)
    all_speakers <- c(all_speakers, speakers)
  }
}

# Get final unique speaker names
unique_speakers <- sort(unique(all_speakers))

# Output the result
print(unique_speakers)





























library(readr)
library(dplyr)
library(stringr)

# Define directory path and target speaker
dir_path <- "data/modified_data/finalized_data"
target_speaker <- "PEREZ"  # Change this to the speaker you're looking for

# List all .csv and .tsv files
transcript_files <- list.files(path = dir_path, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Initialize vector to store file names that contain the target speaker
files_with_speaker <- c()

# Loop through each file
for (file in transcript_files) {
  file_data <- tryCatch({
    if (str_detect(file, "\\.csv$")) {
      read_csv(file, col_types = cols())
    } else {
      read_tsv(file, col_types = cols())
    }
  }, error = function(e) {
    message(paste("Could not read:", file))
    return(NULL)
  })
  
  if (!is.null(file_data) && "speaker_std" %in% names(file_data)) {
    if (target_speaker %in% file_data$speaker_std) {
      files_with_speaker <- c(files_with_speaker, file)
    }
  }
}

# Output the matching file paths
print(files_with_speaker)



file<-"data/modified_data/finalized_data/42.tsv"
df<-read_tsv(file)
write_csv(df, "../53.csv")
file<-"data/modified_data/finalized_data/65.tsv"
df<-read_tsv(file)
write_csv(df, "../77.csv")

















# Load required package
library(dplyr)

# Set the directory containing your transcript files
transcript_folder <- "data/modified_data/finalized_data"  # Change this to your actual path

# Define the old and new speaker labels
old_label <- "MANUEL LOPEZ"
new_label <- "LOPEZ"

# List CSV and TSV files
transcript_files <- list.files(path = transcript_folder, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Loop through each file
for (file in transcript_files) {
  # Detect file format based on extension
  file_ext <- tools::file_ext(file)
  
  # Read the file with appropriate separator
  df <- if (file_ext == "tsv") {
    read.delim(file, stringsAsFactors = FALSE)
  } else {
    read.csv(file, stringsAsFactors = FALSE)
  }
  
  # Modify speaker_std
  if ("speaker_std" %in% names(df)) {
    df$speaker_std[df$speaker_std == old_label] <- new_label
  }
  
  # Write back in original format
  if (file_ext == "tsv") {
    write.table(df, file, sep = "\t", row.names = FALSE, quote = FALSE)
  } else {
    write.csv(df, file, row.names = FALSE)
  }
}












