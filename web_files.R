

#---------------------------Transcript Speakers DF---------------------------

library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

# Define your directory path
transcript_dir <- "./data/modified_data/finalized_data"

# List all transcript files
transcript_files <- list.files(
  transcript_dir,
  pattern = "\\.(csv|tsv)$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Function to extract and clean speakers from each file
extract_speakers_concat <- function(fp) {
  df <- if (str_detect(fp, "\\.csv$")) {
    read_csv(fp, col_types = cols())
  } else {
    read_tsv(fp, col_types = cols())
  }
  
  file_name <- tools::file_path_sans_ext(basename(fp))
  
  if ("speaker_std" %in% names(df)) {
    unique_speakers <- df %>%
      filter(!is.na(speaker_std)) %>%
      distinct(speaker_std) %>%
      filter(!speaker_std %in% c("BACKGROUND", "DESCONOCIDO")) %>%
      pull(speaker_std)
    
    tibble(
      transcript = file_name,
      speakers = paste(unique_speakers, collapse = ", ")
    )
  } else {
    tibble(transcript = file_name, speakers = NA_character_)
  }
}

# Apply to all files and combine
speaker_summary_df <- map_df(transcript_files, extract_speakers_concat)

# View result
print(speaker_summary_df)








# Assume you already have speaker_summary_df (from previous step)

# Step 1: Split speaker string into a list-column
speaker_expanded <- speaker_summary_df %>%
  mutate(speaker_list = str_split(speakers, ",\\s*"))  # split by comma + optional space

# Step 2: Unnest the list into wide format
speaker_wide <- speaker_expanded %>%
  select(transcript, speaker_list) %>%
  unnest_wider(speaker_list, names_sep = "_")

# Optional: rename columns to something like speaker_1, speaker_2, ...
colnames(speaker_wide) <- c("transcript", paste0("speaker_", seq_len(ncol(speaker_wide) - 1)))

# View result
print(speaker_wide)


write_csv(speaker_wide, "data/transcript_speakers_inventory.csv")







file<-read_csv("data/Descriptions.csv")

colnames(file)
































