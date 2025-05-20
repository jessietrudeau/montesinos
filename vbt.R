library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)


# Step 1: Load the actor metadata
actors_df <- read_csv("data/Actors.csv")

# Step 2: Get list of transcript files (both CSV and TSV)
transcript_dir <- "data/modified_data/finalized_data"
files <- list.files(transcript_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Step 3: Function to read a file and extract speaker_std and file info
extract_speaker_data <- function(file) {
  ext <- tools::file_ext(file)
  if (ext == "csv") {
    df <- read_csv(file, show_col_types = FALSE)
  } else {
    df <- read_tsv(file, show_col_types = FALSE)
  }
  df <- df %>%
    filter(!is.na(speaker_std)) %>%
    select(speaker_std) %>%
    distinct() %>%
    mutate(file_id = basename(file))
  return(df)
}

# Step 4: Apply to all files and combine
all_speakers <- bind_rows(lapply(files, extract_speaker_data))

# Step 5: Count number of distinct files each speaker_std appears in (visits)
visit_counts <- all_speakers %>%
  distinct(speaker_std, file_id) %>%
  count(speaker_std, name = "total_visits")

# Step 6: Join with actor metadata to get the 'Type'
visit_summary <- visit_counts %>%
  left_join(actors_df, by = "speaker_std") %>%
  filter(!is.na(Type))  # remove unmatched if needed

# Step 7: Summarize total visits per Type
visits_by_type <- visit_summary %>%
  group_by(Type) %>%
  summarise(total_visits = sum(total_visits), .groups = "drop") %>%
  arrange(desc(total_visits))

# View result
print(visits_by_type)


# Bar chart of total visits by Type
ggplot(visits_by_type, aes(x = reorder(Type, -total_visits), y = total_visits)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Visits by Actor Type",
    x = "Actor Type",
    y = "Total Visits (Distinct Transcripts)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


