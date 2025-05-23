
# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# Step 1: Read the inventory file
file <- "data/Copy of inventory - Transcript inventory.tsv"
inventory <- read_tsv(file)

# Step 2: Identify topic columns
topic_columns <- inventory %>%
  select(starts_with("topic_")) %>%
  colnames()

# Step 3: Count how many transcripts mention each topic (non-NA values)
topic_counts <- inventory %>%
  select(all_of(topic_columns)) %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Topic", values_to = "Transcript_Count")

# Step 4: Clean up topic names for readability
topic_counts <- topic_counts %>%
  mutate(Topic = str_replace_all(Topic, "topic_", ""),
         Topic = str_replace_all(Topic, "_", " "),
         Topic = str_to_title(Topic)) %>%
  arrange(desc(Transcript_Count))

# Step 5: Plot histogram
ggplot(topic_counts, aes(x = reorder(Topic, Transcript_Count), y = Transcript_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Number of Transcripts Discussing Each Topic",
       x = "Topic",
       y = "Transcript Count") +
  theme_minimal()




library(readr)
library(dplyr)
library(ggplot2)

# Read in File
file_path <- "data/count_results_all.tsv"
word_data <- read_tsv(file_path)

# Exclude 'DESCONOCIDO'
word_data <- word_data %>% filter(Speaker != "DESCONOCIDO")

# Compute proportions (Exluding the use of existing Propoetion Column)
word_data <- word_data %>%
  mutate(Proportion = (`Total Word Count` / sum(`Total Word Count`)) * 100)

# Plot bar chart
ggplot(word_data, aes(x = reorder(Speaker, Proportion), y = Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(title = "Word Count Proportion by Speaker",
       x = "Speaker",
       y = "Proportion of Words Spoken (%)") +
  theme_minimal()+
  theme(axis.text.y = element_text(size = 5))


# Filter out 'MONTESINOS' and 'DESCONOCIDO'
filtered_data <- word_data %>%
  filter(!(Speaker %in% c("MONTESINOS", "DESCONOCIDO")))

# Recalculate proportions based on filtered total word counts
filtered_data <- filtered_data %>%
  mutate(Proportion = (`Total Word Count` / sum(`Total Word Count`)) * 100)

# Select top 20 speakers by recalculated proportion
top20_data <- filtered_data %>%
  arrange(desc(Proportion)) %>%
  slice_head(n = 20)

# Plot bar chart
ggplot(top20_data, aes(x = reorder(Speaker, Proportion), y = Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Speakers by Recalculated Word Count Proportion (Excluding MONTESINOS & DESCONOCIDO)",
       x = "Speaker",
       y = "Proportion of Words Spoken (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
file<-"output/speaker_frequency_results(all).csv"
df <- read_csv(file)

# Filter out 'Montesinos' and 'Desconocido'
df_filtered <- df %>%
  filter(!(Speaker %in% c("MONTESINOS", "DESCONOCIDO")))

# Create the bar graph
ggplot(df_filtered, aes(x = reorder(Speaker, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Frequency of Speakers",
       x = "Speaker",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Keep only the top 20 speakers based on frequency
df_top20 <- df_filtered %>%
  arrange(desc(Frequency)) %>%
  slice_head(n = 20)

# Plot top 20
ggplot(df_top20, aes(x = reorder(Speaker, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Top 20 Speakers by Frequency",
       x = "Speaker",
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)


# Read in 'topic_vmt_avg_count.csv' file
file<-"output/topic_vmt_avg_count.csv"
df <- read.csv(file)

# Remove 'topic_' prefix from 'Topic' column
df$Topic <- gsub("topic_", "", df$Topic)

# Replace underscores with spaces in 'Topic' column
df$Topic <- gsub("_", " ", df$Topic)

# Capitalize first letter of each item in 'Topic' column
df$Topic <- str_to_title(df$Topic)  

# Convert 'Topic' to factor to maintain order
df$Topic <- factor(df$Topic, levels = df$Topic)

# Reshape data using pivot_longer()
df_long <- df %>%
  pivot_longer(cols = c(Average.Conversation.Word.Count, Montesinos.Average.Word.Count),
               names_to = "Word_Count_Type",
               values_to = "Word_Count")

# Create a grouped bar plot with y-axis limit set to 25,000 and removed x-axis lines
ggplot(df_long, aes(x = Topic, y = Word_Count, fill = Word_Count_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.4) +
  labs(title = "Average Word Count per Topic",
       x = "Topic",
       y = "Word Count",
       fill = "Word Count Type") +
  scale_fill_manual(values = c("Average.Conversation.Word.Count" = "deepskyblue2", 
                               "Montesinos.Average.Word.Count" = "orange")) +
  scale_y_continuous(limits = c(0, 25000)) +  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  
    panel.grid.major.x = element_blank(),  
    panel.grid.minor.x = element_blank()   
  )



# Define the path to the finalized_data folder
data_path <- "data/modified_data/finalized_data"

# Get a list of CSV and TSV files from finalized_data without setting it as the working directory
csv_files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)
tsv_files <- list.files(path = data_path, pattern = "\\.tsv$", full.names = TRUE)

# Combine file lists
all_files <- c(csv_files, tsv_files)

# Load necessary libraries
#install.packages("readr")
#install.packages("stringr")
#install.packages("ggplot2")
library(ggplot2)
library(readr)  # For reading CSV & TSV files
library(stringr)  # For text processing

# Initialize total word count and file count
total_word_count <- 0
file_count <- 0

# Function to count words in the 'speech' column safely
count_words <- function(text) {
  if (is.null(text) || all(is.na(text))) {
    return(0)  # Return 0 if text is NULL or all NA
  }
  text <- na.omit(text)  # Remove NA values
  sum(str_count(text, "\\S+"))  # Count words in non-NA text
}

# Loop through each file
for (file in all_files) {
  # Read the file and handle errors
  df <- tryCatch({
    if (grepl("\\.csv$", file)) {
      read_csv(file, show_col_types = FALSE)  # Read CSV
    } else if (grepl("\\.tsv$", file)) {
      read_tsv(file, show_col_types = FALSE)  # Read TSV
    }
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    return(NULL)
  })
  
  # Check if file was successfully read and 'speech' column exists
  if (!is.null(df) && "speech" %in% colnames(df)) {
    # Calculate total words in 'speech' column
    file_word_count <- count_words(df$speech)
    
    # Debugging: Print word count for each file
    cat("File:", basename(file), "- Word Count:", file_word_count, "\n")
    
    # Update total word count and file count
    total_word_count <- total_word_count + file_word_count
    file_count <- file_count + 1
  } else {
    cat("Skipping file (missing 'speech' column):", basename(file), "\n")
  }
}

# Calculate the average word count per file
average_word_count <- ifelse(file_count > 0, total_word_count / file_count, NA)

# Print result
cat("Total Word Count:", total_word_count, "\n")
cat("Number of Files Processed:", file_count, "\n")
cat("Average Word Count per File:", average_word_count, "\n")

# Initialize a data frame to store file names and word counts
word_counts <- data.frame(File = character(), Word_Count = numeric(), stringsAsFactors = FALSE)

# Loop through each file again to store word counts
for (file in all_files) {
  df <- tryCatch({
    if (grepl("\\.csv$", file)) {
      read_csv(file, show_col_types = FALSE)
    } else if (grepl("\\.tsv$", file)) {
      read_tsv(file, show_col_types = FALSE)
    }
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    return(NULL)
  })
  
  if (!is.null(df) && "speech" %in% colnames(df)) {
    file_word_count <- count_words(df$speech)
    
    # Store results in the data frame
    word_counts <- rbind(word_counts, data.frame(File = basename(file), Word_Count = file_word_count))
  }
}

# Calculate average word count
average_word_count <- mean(word_counts$Word_Count, na.rm = TRUE)

# Create histogram
ggplot(word_counts, aes(x = Word_Count)) +
  geom_histogram(binwidth = 100, fill = "blue", alpha = 0.7, color = "black") +
  geom_vline(aes(xintercept = average_word_count), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Word Counts per File",
       x = "Word Count",
       y = "Frequency") +
  theme_minimal()



# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# Define file paths
inventory_file <- "data/Copy of inventory - Transcript inventory.tsv"
directory_path <- "data/modified_data/finalized_data"

# Read the inventory data
inventory_data <- read_tsv(inventory_file, col_types = cols())

# List of topic columns
topic_columns <- c("topic_referendum", "topic_ecuador", "topic_lucchetti_factory", "topic_municipal98", 
                   "topic_reelection", "topic_miraflores", "topic_canal4", "topic_media", "topic_promotions", 
                   "topic_ivcher", "topic_foreign", "topic_wiese", "topic_public_officials", "topic_safety", "topic_state_capture")

# Initialize word count storage
topic_word_count <- setNames(rep(0, length(topic_columns)), topic_columns)

# Function to count words in a transcript file (from the 'speech' column)
count_words_in_transcript <- function(file_path) {
  if (!file.exists(file_path)) return(0)
  
  # Read the transcript file
  transcript_data <- read_tsv(file_path, col_types = cols(), na = c("", "NA"))
  
  # Check if the 'speech' column exists
  if (!"speech" %in% colnames(transcript_data)) return(0)
  
  # Extract valid speeches (ignore missing values)
  valid_speeches <- transcript_data %>%
    filter(!is.na(speech)) %>%
    pull(speech)
  
  # Calculate total word count from the 'speech' column
  total_words <- sum(str_count(valid_speeches, "\\S+"))
  return(total_words)
}

# Iterate over transcript files and compute word counts
for (i in 1:nrow(inventory_data)) {
  transcript_id <- inventory_data$n[i]
  
  # Construct file path (assuming files are named as "n.tsv")
  file_path <- file.path(directory_path, paste0(transcript_id, ".tsv"))
  
  # Compute word count for the transcript
  transcript_word_count <- count_words_in_transcript(file_path)
  
  # Assign word count to relevant topics
  for (topic in topic_columns) {
    if (!is.na(inventory_data[[topic]][i]) && inventory_data[[topic]][i] == "x") {
      topic_word_count[topic] <- topic_word_count[topic] + transcript_word_count
    }
  }
}

# Convert results to a data frame
word_count_df <- data.frame(
  Topic = names(topic_word_count),
  Word_Count = unlist(topic_word_count),
  stringsAsFactors = FALSE
)

# Save results to CSV
output_file <- "word_count_by_topic.csv"
write_csv(word_count_df, output_file)

# Print summary
print(word_count_df)

# Histogram of Word Count Per Topic
ggplot(word_count_df, aes(x = reorder(Topic, -Word_Count), y = Word_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Word Count Per Topic", x = "Topic", y = "Word Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate the average word count across all transcripts
average_word_count_all <- sum(word_count_df$Word_Count) / nrow(inventory_data)

# Print average word count
print(paste("Average Word Count Across All Transcripts:", round(average_word_count_all, 2)))

# Calculate the average word count across all topics
average_word_count_topics <- sum(word_count_df$Word_Count) / length(topic_columns)

# Print average word count per topic
print(paste("Average Word Count Across All Topics:", round(average_word_count_topics, 2)))

ggplot(word_count_df, aes(x = reorder(Topic, -Word_Count), y = Word_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = average_word_count_topics, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Word Count Per Topic", x = "Topic", y = "Word Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = 1, y = average_word_count_all + 100, label = paste("Avg:", round(average_word_count_topics, 2)), color = "red", hjust = 0)

print(paste("Total Word Count Across All Transcripts:", sum(word_count_df$Word_Count)))





# Define the path to the finalized_data folder (relative to current working directory)
finalized_data_path <- "data/modified_data/finalized_data"

library(readr)   # For reading CSV & TSV files
library(stringr) # For text processing
library(dplyr)   # For data manipulation
library(ggplot2) # For visualization
library(stringi)  # For accent removal


# Get a list of CSV and TSV files in the finalized_data directory
csv_files <- list.files(path = finalized_data_path, pattern = "\\.csv$", full.names = TRUE)
tsv_files <- list.files(path = finalized_data_path, pattern = "\\.tsv$", full.names = TRUE)

# Combine file lists
all_files <- c(csv_files, tsv_files)

# Function to count words in the 'speech' column safely
count_words <- function(text) {
  if (is.null(text) || all(is.na(text))) {
    return(0)  # Return 0 if text is NULL or all NA
  }
  text <- na.omit(text)  # Remove NA values
  sum(str_count(text, "\\S+"))  # Count words in non-NA text
}

# Initialize an empty list to store data frames
all_speaker_data <- list()

# Loop through each file and accumulate results
for (file in all_files) {
  df <- tryCatch({
    if (grepl("\\.csv$", file)) {
      read_csv(file, show_col_types = FALSE)
    } else if (grepl("\\.tsv$", file)) {
      read_tsv(file, show_col_types = FALSE)
    }
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    return(NULL)
  })
  
  # Proceed only if the file was successfully read and contains required columns
  if (!is.null(df) && all(c("speech", "speaker_std") %in% colnames(df))) {
    
    # Process data to count words per speaker
    df <- df %>%
      filter(!is.na(speech) & !is.na(speaker_std)) %>%
      group_by(speaker_std) %>%
      summarise(
        Total_Words = sum(count_words(speech)),
        Appearances = n(),
        .groups = "drop"
      )
    
    # Store the processed data in a list
    all_speaker_data[[basename(file)]] <- df  # Use basename(file) for readability
  } else {
    cat("Skipping file (missing required columns):", basename(file), "\n")
  }
}

# Combine all accumulated data into a single data frame
if (length(all_speaker_data) > 0) {
  speaker_word_counts <- bind_rows(all_speaker_data) %>%
    mutate(speaker_std = str_trim(str_to_upper(speaker_std)),
           speaker_std = stri_trans_general(speaker_std, "Latin-ASCII")) %>%  # Normalize
    filter(!(speaker_std %in% c("DESCONOCIDO", "ALBARRACíN"))) %>%  
    group_by(Speaker = speaker_std) %>%
    summarise(
      Total_Words = sum(Total_Words),
      Appearances = sum(Appearances),
      .groups = "drop"
    ) %>%
    mutate(Average_Words_Per_Appearance = Total_Words / Appearances)
} else {
  speaker_word_counts <- data.frame()
  cat("No valid data found in any files.\n")
}

# Print summary
cat("Total Unique Speakers:", nrow(speaker_word_counts), "\n")

# Display the final aggregated results
print(speaker_word_counts)

# Sort data by Average Words Per Appearance (Descending)
speaker_word_counts <- speaker_word_counts %>%
  arrange(desc(Average_Words_Per_Appearance))

# Create the bar plot with all speakers on the x-axis
ggplot(speaker_word_counts, aes(x = reorder(Speaker, -Average_Words_Per_Appearance), y = Average_Words_Per_Appearance)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(title = "Words Per Appearance by Speaker",
       x = "Speaker",
       y = "Average Words Per Appearance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10)  # Keep y-axis labels readable
  )



# Create the bar plot showing only the top 20 speakers by average words per appearance
top20_speakers <- speaker_word_counts %>%
  arrange(desc(Average_Words_Per_Appearance)) %>%
  slice_head(n = 20)


ggplot(top20_speakers, aes(x = reorder(Speaker, -Average_Words_Per_Appearance), y = Average_Words_Per_Appearance)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(title = "Top 20 Speakers by Average Words Per Appearance",
       x = "Speaker",
       y = "Average Words Per Appearance") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 8),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 10)  # Keep y-axis labels readable
  )

raw_names <- bind_rows(all_speaker_data) %>%
  distinct(speaker_std) %>%
  arrange(speaker_std)

print(raw_names)











# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vistime)
library(lubridate)
library(stringr)  # For counting speakers

# Read the TSV file
file_path <- "data/Copy of inventory - Transcript inventory.tsv"
data <- read_tsv(file_path)

# Convert 'date' column to Date format and filter out dates before 1990
data <- data %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(date) & date >= as.Date("1990-01-01"))

# Count number of speakers
data <- data %>%
  mutate(num_speakers = ifelse(is.na(speakers), 0, str_count(speakers, ",") + 1))

# Select topic columns and reshape into long format
topic_columns <- names(data)[grepl("^topic_", names(data))]

long_data <- data %>%
  select(n, date, speakers, num_speakers, all_of(topic_columns)) %>%
  pivot_longer(cols = all_of(topic_columns), names_to = "topic", values_to = "present") %>%
  filter(!is.na(present)) %>%
  mutate(topic = gsub("topic_", "", topic))  # Remove "topic_" prefix for clarity

# Define a custom gradient with multiple breakpoints
ggplot(long_data, aes(x = date, y = topic, color = num_speakers)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradientn(colors = c("blue", "skyblue", "yellow", "lightgreen", "seagreen"),
                        values = scales::rescale(c(min(long_data$num_speakers, na.rm = TRUE), 
                                                   quantile(long_data$num_speakers, 0.25, na.rm = TRUE), 
                                                   median(long_data$num_speakers, na.rm = TRUE), 
                                                   quantile(long_data$num_speakers, 0.75, na.rm = TRUE), 
                                                   max(long_data$num_speakers, na.rm = TRUE)))) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y-%m") +
  labs(title = "Timeline of Conversations by Topic",
       subtitle = "Based on transcript inventory",
       x = "Date",
       y = "Topic",
       color = "Number of Speakers") +
  theme_minimal()
















# --- Load libraries ---
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vistime)
library(lubridate)
library(stringr)

# --- Step 1: Read inventory data ---
file_path <- "data/Copy of inventory - Transcript inventory.tsv"
data <- read_tsv(file_path)

# --- Step 2: Format date ---
data <- data %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(date) & date >= as.Date("1990-01-01"))

# --- Step 3: Calculate number of speakers ---
data <- data %>%
  mutate(num_speakers = ifelse(is.na(speakers), 0, str_count(speakers, ",") + 1))

# --- Step 4: Calculate word counts per transcript ---
# Define path to finalized transcripts
transcript_dir <- "data/modified_data/finalized_data"

# Helper function to count words from file
count_words <- function(file_path) {
  if (!file.exists(file_path)) return(NA)
  df <- tryCatch(read_tsv(file_path, show_col_types = FALSE), error = function(e) return(NULL))
  if (is.null(df) || !"speech" %in% names(df)) return(NA)
  sum(str_count(na.omit(df$speech), "\\S+"))
}

# Apply function to each transcript file (matched by 'n')
data <- data %>%
  rowwise() %>%
  mutate(file_path = file.path(transcript_dir, paste0(n, ".tsv")),
         word_count = count_words(file_path)) %>%
  ungroup()

# --- Step 5: Reshape topics into long format ---
topic_columns <- names(data)[grepl("^topic_", names(data))]

long_data <- data %>%
  select(n, date, speakers, word_count, all_of(topic_columns)) %>%
  pivot_longer(cols = all_of(topic_columns), names_to = "topic", values_to = "present") %>%
  filter(!is.na(present)) %>%
  mutate(topic = gsub("topic_", "", topic))

# --- Step 6: Plot with word count as color ---
ggplot(long_data, aes(x = date, y = topic, color = word_count)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradientn(colors = c("blue", "skyblue", "yellow", "lightgreen", "seagreen"),
                        values = scales::rescale(c(min(long_data$word_count, na.rm = TRUE), 
                                                   quantile(long_data$word_count, 0.25, na.rm = TRUE), 
                                                   median(long_data$word_count, na.rm = TRUE), 
                                                   quantile(long_data$word_count, 0.75, na.rm = TRUE), 
                                                   max(long_data$word_count, na.rm = TRUE)))) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y-%m") +
  labs(title = "Timeline of Conversations by Topic",
       subtitle = "Colored by Transcript Word Count",
       x = "Date",
       y = "Topic",
       color = "Word Count") +
  theme_minimal()



long_data %>%
  filter(!is.na(word_count)) %>%
  ggplot(aes(x = date, y = topic, color = word_count)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradientn(
    colors = c("blue", "skyblue", "yellow", "lightgreen", "darkgreen"),
    values = scales::rescale(c(
      min(long_data$word_count, na.rm = TRUE),
      quantile(long_data$word_count, 0.25, na.rm = TRUE),
      median(long_data$word_count, na.rm = TRUE),
      quantile(long_data$word_count, 0.75, na.rm = TRUE),
      max(long_data$word_count, na.rm = TRUE)
    ))
  ) +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months", date_labels = "%Y-%m") +
  labs(
    title = "Timeline of Conversations by Topic",
    subtitle = "Colored by Transcript Word Count",
    x = "Date",
    y = "Topic",
    color = "Word Count"
  ) +
  theme_minimal()



sum(is.na(long_data$word_count))


long_data %>%
  filter(is.na(word_count)) %>%
  select(n, date, topic)


missing_data_check <- inventory_data %>%
  mutate(file_path = file.path("data/modified_data/finalized_data", paste0(n, ".tsv")),
         file_exists = file.exists(file_path)) %>%
  filter(n %in% long_data$n[is.na(long_data$word_count)])

for (i in 1:nrow(missing_data_check)) {
  file <- missing_data_check$file_path[i]
  cat("Checking:", basename(file), "\n")
  
  if (!file.exists(file)) {
    cat(" - File does not exist\n\n")
  } else {
    df <- tryCatch(read_tsv(file, show_col_types = FALSE), error = function(e) return(NULL))
    
    if (is.null(df)) {
      cat(" - Read error or malformed file\n\n")
    } else if (!"speech" %in% colnames(df)) {
      cat(" - Missing 'speech' column\n\n")
    } else if (all(is.na(df$speech))) {
      cat(" - 'speech' column exists but all values are NA\n\n")
    } else {
      cat(" - Unexpected: Check manually\n\n")
    }
  }
}








# Create a summary table counting occurrences of each unique value in "Type_Merged"
file <- "data/Copy of Master - transcript notes - Actors.tsv"
df <- read_tsv(file)


#Verify column names
colnames(df)

# Ensure column names are clean
colnames(df) <- gsub(" ", "_", colnames(df))   # Replace spaces with underscores
colnames(df) <- gsub("'", "", colnames(df))    # Remove apostrophes

# Fix NA values: Ensure we replace only when necessary
df$Type_Merged <- ifelse(!is.na(df$Montesinos_inner_circle) & df$Montesinos_inner_circle == "X", 
                         "Inner Circle", 
                         ifelse(!is.na(df$Type), df$Type, NA))

# Verify the result
head(df[, c("Type", "Montesinos_inner_circle", "Type_Merged")])

# Define output file path
output_file<-"data/transcript_notes_cleaned.tsv"

# Write the cleaned dataset to a new CSV file
write_csv(df, output_file)

# Confirm the file was saved
message("Cleaned dataset saved as: ", output_file)


type_summary <- df %>%
  group_by(Type_Merged) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Print summary table
print(type_summary)




library(tidyverse)

# Set the directory where transcript files are stored
transcript_dir <- "data/modified_data/finalized_data"

# Get list of all CSV and TSV files
transcript_files <- list.files(path = transcript_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Function to detect delimiter and read file correctly
read_transcript_file <- function(file_path) {
  if (grepl("\\.csv$", file_path)) {
    return(read_csv(file_path, col_types = cols()))
  } else if (grepl("\\.tsv$", file_path)) {
    return(read_tsv(file_path, col_types = cols()))
  } else {
    return(NULL)  # In case of an unexpected format
  }
}

# Initialize an empty dataframe to store speaker visits
speaker_visits <- tibble(speaker_std = character(), file_name = character())

# Loop through each transcript file and extract unique speaker_std values
for (file in transcript_files) {
  # Read file using the appropriate function
  df <- read_transcript_file(file)
  
  # Check if 'speaker_std' column exists in the file
  if (!is.null(df) && "speaker_std" %in% colnames(df)) {
    # Store unique speakers per file
    unique_speakers <- df %>%
      select(speaker_std) %>%
      distinct() %>%
      mutate(file_name = basename(file))
    
    # Append to the master visits dataframe
    speaker_visits <- bind_rows(speaker_visits, unique_speakers)
  }
}

# Count unique appearances per speaker (number of transcript files they appear in)
speaker_counts <- speaker_visits %>%
  group_by(speaker_std) %>%
  summarize(visits = n(), .groups = "drop")

# Load the master actors dataset
actors_file <- "data/transcript_notes_cleaned.tsv"
actors_data <- read_csv(actors_file, col_types = cols())

# Clean column names to match modifications
colnames(actors_data) <- colnames(actors_data) %>% str_replace_all(" ", "_") %>% str_replace_all("'", "")

# Merge visits data with actor classifications
merged_data <- speaker_counts %>%
  left_join(actors_data %>% select(speaker_std, Type_Merged), by = "speaker_std")

# Count total visits by actor classification
visit_summary <- merged_data %>%
  group_by(Type_Merged) %>%
  summarize(total_visits = sum(visits, na.rm = TRUE), unique_speakers = n(), .groups = "drop")

# Display results
print(visit_summary)

# Optionally, save to CSV
write_csv(visit_summary, "data/visit_summary_by_type.csv")

# Load the summarized visit data
visit_summary <- read_csv("data/visit_summary_by_type.csv", col_types = cols())
visit_summary <- visit_summary %>% filter(!is.na(Type_Merged))

# Create a bar plot to visualize the total visits for each type
ggplot(visit_summary, aes(x = reorder(Type_Merged, -total_visits), y = total_visits, fill = Type_Merged)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  # Bar plot with total_visits as height
  labs(title = "Total Visits by Type", 
       x = "Type of Individual", 
       y = "Total Visits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Create a new column categorizing Inner Circle vs. Outer Circle
visit_summary <- visit_summary %>%
  filter(!is.na(Type_Merged)) %>%
  mutate(circle_group = ifelse(Type_Merged == "Inner Circle", "Inner Circle", "Outer Circle"))

# Summarize total visits for each category
circle_summary <- visit_summary %>%
  group_by(circle_group) %>%
  summarize(total_visits = sum(total_visits, na.rm = TRUE), .groups = "drop")

# Create a bar plot to compare Inner Circle vs. Outer Circle
ggplot(circle_summary, aes(x = circle_group, y = total_visits, fill = circle_group)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  labs(title = "Comparison of Visits: Inner Circle vs. Outer Circle", 
       x = "Group", 
       y = "Total Visits") +
  theme_minimal() +
  scale_fill_manual(values = c("Inner Circle" = "red", "Outer Circle" = "blue"))  # Custom colors









