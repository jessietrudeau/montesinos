
# Word Count Proportion (excluding 'desconoocido')
```{r}

install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

library(readr)
library(dplyr)
library(ggplot2)

# Read in File
file_path <- "C:/Users/agsotopl/OneDrive - Syracuse University/Documents/Montesinos tests/count_results_all.tsv"
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

```

# Speaker Frequency
```{r} 

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the dataset
file<-"C:/Users/agsotopl/OneDrive - Syracuse University/Documents/Montesinos tests/speaker_frequency_results(all).csv"
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

```

# Word Count Per Topic
```{r}

# Install & Library necessary packages
install.packages("tidyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

# Read in 'topic_vmt_avg_count.csv' file
file<-"C:/Users/agsotopl/OneDrive - Syracuse University/Documents/Montesinos tests/topic_vmt_avg_count.csv"
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

```

# Histograms

## Average Length of Conversations
```{r}

setwd("C:/Users/agsotopl/Downloads/montesinos/data/modified_data/finalized_data")
list.files()

# Load necessary libraries
install.packages("readr")
install.packages("stringr")
install.packages("ggplot2")
library(ggplot2)
library(readr)  # For reading CSV & TSV files
library(stringr)  # For text processing


# Get a list of CSV and TSV files
csv_files <- list.files(pattern = "\\.csv$")
tsv_files <- list.files(pattern = "\\.tsv$")

# Combine file lists
all_files <- c(csv_files, tsv_files)

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
    cat("File:", file, "- Word Count:", file_word_count, "\n")
    
    # Update total word count and file count
    total_word_count <- total_word_count + file_word_count
    file_count <- file_count + 1
  } else {
    cat("Skipping file (missing 'speech' column):", file, "\n")
  }
}

# Calculate the average word count per file
average_word_count <- ifelse(file_count > 0, total_word_count / file_count, NA)

# Print result
cat("Total Word Count:", total_word_count, "\n")
cat("Number of Files Processed:", file_count, "\n")
cat("Average Word Count per File:", average_word_count, "\n")




setwd("C:/Users/agsotopl/Downloads/montesinos/data/modified_data/finalized_data")
list.files()

# Load necessary libraries
install.packages("readr")
install.packages("stringr")
install.packages("ggplot2")
library(ggplot2)
library(readr)  # For reading CSV & TSV files
library(stringr)  # For text processing


# Get a list of CSV and TSV files
csv_files <- list.files(pattern = "\\.csv$")
tsv_files <- list.files(pattern = "\\.tsv$")

# Combine file lists
all_files <- c(csv_files, tsv_files)

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
    cat("File:", file, "- Word Count:", file_word_count, "\n")
    
    # Update total word count and file count
    total_word_count <- total_word_count + file_word_count
    file_count <- file_count + 1
  } else {
    cat("Skipping file (missing 'speech' column):", file, "\n")
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

# Loop through each file
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
    word_counts <- rbind(word_counts, data.frame(File = file, Word_Count = file_word_count))
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


```

## Average Conversation Length by Topic
```{r}
# Load required libraries
library(dplyr)
library(readr)
library(stringr)

# Define file paths
inventory_file <- "C:/Users/agsotopl/Downloads/Copy of inventory - Transcript inventory.tsv"
directory_path <- "C:/Users/agsotopl/Downloads/montesinos/data/modified_data/finalized_data"

# Read the inventory data
inventory_data <- read_tsv(inventory_file, col_types = cols())

# List of topic columns
topic_columns <- c("topic_referendum", "topic_ecuador", "topic_lucchetti_factory", "topic_municipal98", 
                   "topic_reelection", "topic_miraflores", "topic_canal4", "topic_media", "topic_promotions", 
                   "topic_ivcher", "topic_foreign", "topic_wiese", "topic_public_officials", "topic_safety", "topic_state_capture")

# Initialize word count storage
topic_word_count <- setNames(rep(0, length(topic_columns)), topic_columns)

# Function to count words in a transcript file
count_words_in_transcript <- function(file_path) {
  if (!file.exists(file_path)) return(0)
  
  # Read the file
  transcript_data <- read_tsv(file_path, col_types = cols(), na = c("", "NA"))
  
  # Check if the necessary columns exist
  if (!all(c("speaker_std", "speech") %in% colnames(transcript_data))) return(0)
  
  # Filter out background speakers
  valid_speeches <- transcript_data %>%
    filter(!is.na(speech), speaker_std != "BACKGROUND") %>%
    pull(speech)
  
  # Calculate total word count
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


```

# Balance of Conversations
```{r}




```


# Average Conversation Length by Topic
```{r}




```



