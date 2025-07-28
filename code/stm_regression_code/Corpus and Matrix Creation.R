
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




#STM

# Install and load the required libraries
install.packages("stm")
install.packages(c("geometry", "Rtsne", "rsvd"))
library(geometry)
library(Rtsne)
library(rsvd)
library(stm)

# Convert quanteda dfm to STM format
dfm_stm <- convert(dfm_general, to = "stm")

# Define the number of topics (e.g., 5 topics)
num_topics <- 0

# Fit the STM model
stm_model <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = num_topics,
  prevalence = ~ speaker,  # Include metadata if available
  max.em.its = 75,
  data = dfm_stm$meta
)

# Inspect top words for each topic
labelTopics(stm_model)

# Plot topic proportions
plot.STM(stm_model, type = "summary", labeltype = "frex")



# Tidytext Analysis

# Install and load libraries
install.packages("tidytext")
library(tidytext)

# Convert speech data to tidy format
tidy_text <- text_data %>%
  unnest_tokens(word, speech)

# Join with sentiment lexicon
sentiment_scores <- tidy_text %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE)

# Aggregate sentiment
sentiment_summary <- sentiment_scores %>%
  group_by(sentiment) %>%
  summarize(total = sum(n))

# Visualize sentiment distribution
library(ggplot2)
ggplot(sentiment_summary, aes(x = sentiment, y = total, fill = sentiment)) +
  geom_col() +
  theme_minimal()




#Setnimentr Text Analysis

# Install and load sentimentr
install.packages("sentimentr")
library(sentimentr)

# Analyze sentiment at sentence level
sentiment_results <- sentiment(text_data$speech)

# View summary
summary(sentiment_results)

# Visualize sentiment trajectory
plot(sentiment_results)





