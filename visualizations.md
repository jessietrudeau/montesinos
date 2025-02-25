
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

setwd("C:/Users/agsotopl/OneDrive - Syracuse University/Documents/GitHub/montesinos/montesinos/data/modified_data/finalized_data")
list.files()

# Load necessary libraries
install.packages("readr")
install.packages("stringr")
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

```

Average Conversation Length by Topic
```{r}




```

Balance of Conversations
```{r}




```

