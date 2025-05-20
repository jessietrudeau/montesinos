
#Normalize Data

library(ggplot2)
library(dplyr)
library(readr)

# Load the data
df <- read_csv("../../data/babel_batches/results_1.csv")

# Drop rows with missing values in key columns
df_clean <- df %>%
  filter(!is.na(sentence_id), !is.na(emotion_pred), !is.na(emotion_pred_name))

# Normalize sentence position within each conversation
df_clean <- df_clean %>%
  group_by(file_id) %>%
  mutate(normalized_position = (sentence_id - min(sentence_id)) / (max(sentence_id) - min(sentence_id))) %>%
  ungroup()






#Overall Emotion

ggplot(df_clean, aes(x = normalized_position, y = emotion_pred, color = emotion_pred_name)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  labs(
    title = "Overall Emotion Distribution Over Conversation Duration",
    x = "Normalized Conversation Progress",
    y = "Emotion Code"
  ) +
  theme_minimal()








#By Conversation

# Optionally sample a few file_ids for cleaner display
#set.seed(42)
#sampled_files <- sample(unique(df_clean$file_id), )
#df_sample <- df_clean %>% filter(file_id %in% sampled_files)

ggplot(df_clean, aes(x = normalized_position, y = emotion_pred, color = emotion_pred_name)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  facet_wrap(~ file_id, scales = "free_y") +
  labs(
    title = "Emotion Distribution Across Sampled Conversations",
    x = "Normalized Position",
    y = "Emotion Code"
  ) +
  theme_minimal()









#By Topic

# Optionally sample a few topics for clarity
#set.seed(42)
#sampled_topics <- sample(unique(df_clean$topics), 8)
#df_topic_sample <- df_clean %>% filter(topics %in% sampled_topics)

ggplot(df_clean, aes(x = normalized_position, y = emotion_pred, color = emotion_pred_name)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE) +
  facet_wrap(~ topics, scales = "free_y") +
  labs(
    title = "Emotion Distribution Across Sampled Topics",
    x = "Normalized Position",
    y = "Emotion Code"
  ) +
  theme_minimal()











