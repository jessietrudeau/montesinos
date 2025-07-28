

# 1. Load required libraries
library(quanteda)   # corpus + dfm
library(dplyr)      # data manipulation
library(tidyr)      # pivoting
library(readr)      # fast CSV/TSV reading

# 2. Read your inventory and build lookups for topic & date
inventory <- read_csv(
  "data/Updated Inventory & Descriptions/Descriptions.csv",
  col_types = cols(.default = "c")
)

# 2a. Date lookup: one row per transcript ID
date_lookup <- inventory %>%
  select(n, date)

# 2b. Topic lookup: collapse all flagged topic_* columns into a single string per n
topic_lookup <- inventory %>%
  select(n, starts_with("topic_")) %>%
  pivot_longer(
    cols      = starts_with("topic_"),
    names_to  = "topic",
    values_to = "flag"
  ) %>%
  filter(!is.na(flag) & flag != "") %>%     # keep only flagged topics
  group_by(n) %>%
  summarize(
    topic = paste(topic, collapse = "; "),
    .groups = "drop"
  )

# 3. Read your Actors master list and build an actor‐type lookup
actors <- read_csv(
  "data/Updated Inventory & Descriptions/Actors.csv",
  col_types = cols(.default = "c")
)

actor_lookup <- actors %>%
  select(speaker_std, Type) %>%  # capital “T”
  distinct() %>%
  rename(type = Type)

# 4. Define the folder containing your transcript files
data_path <- "data/modified_data/finalized_data/"
file_list <- list.files(
  path       = data_path,
  pattern    = "\\.(csv|tsv)$",
  full.names = TRUE
)

# 5. Custom reader: import each CSV/TSV and tag it with its numeric ID (n)
read_text_data <- function(file) {
  df <- if (grepl("\\.csv$", file)) {
    read_csv(file, col_types = cols(.default = "c"))
  } else {
    read_tsv(file, col_types = cols(.default = "c"))
  }
  df$file <- basename(file)                     # e.g. "97.csv"
  df$n    <- gsub("\\.(csv|tsv)$", "", df$file)  # e.g. "97"
  return(df)
}

# 6. Read all transcripts, combine, and join topic, date & actor info
text_data <- file_list %>%
  lapply(read_text_data) %>%
  bind_rows() %>%
  left_join(topic_lookup, by = "n") %>%   # adds 'topic'
  left_join(date_lookup,  by = "n") %>%   # adds 'date'
  left_join(actor_lookup, by = "speaker_std")  # adds 'type'

# 7. Sanity checks
required_cols <- c("speech", "topic", "date", "type")
missing <- setdiff(required_cols, colnames(text_data))
if (length(missing) > 0) {
  stop("ERROR: Missing columns in text_data: ", paste(missing, collapse = ", "))
}

# 8. Remove stage directions from the speech text
remove_stage_directions <- function(txt) {
  gsub("\\[.*?\\]|\\(.*?\\)", "", txt)
}
text_data$speech <- sapply(text_data$speech, remove_stage_directions)

# 9. Build a Quanteda corpus
#    Now each document carries doc‐vars: file, n, topic, date, type, speaker, speaker_std, etc.
corpus_data <- corpus(text_data, text_field = "speech")

# 10. Tokenize & preprocess (Spanish)
tokens_data <- tokens(
  corpus_data,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("es")) %>%
  tokens_wordstem(language = "es")

# 11. Create Document–Feature Matrices
#     a) General DFM (one document per transcript row)
dfm_general <- dfm(tokens_data)

#     b) Turn‐by‐turn DFM (one document per speaker)
dfm_turns <- dfm_group(dfm(tokens_data), groups = text_data$speaker)

# 12. Save the DFMs for later analysis
saveRDS(dfm_general, "data/dfm_general.rds")
saveRDS(dfm_turns,   "data/dfm_turns.rds")

# 13. Display brief summaries
print(summary(dfm_general))
print(summary(dfm_turns))




