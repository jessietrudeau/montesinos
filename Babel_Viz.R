
library(dplyr)


res<-"data/babel_batches/results_1.csv"
soft<-"data/babel_batches/softmax_1.csv"


results<-read_csv(res)
softmax<-read_csv(soft)

str(softmax)
str(results)


merged_df_1 <- full_join(results, softmax, by = "sentence_id")


# Sample 2000 rows (without replacement)
set.seed(123)  # Optional: for reproducibility
sample_df <- merged_df_1 %>% 
  sample_n(2000)

# Write to CSV
write.csv(sample_df, "data/babel_batches/sample_df.csv", row.names = FALSE)


library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Normalize sentence position within each transcript
merged_df <- merged_df %>%
  group_by(file_id) %>%
  mutate(normalized_position = (rank(sentence_id) - 1) / (n() - 1)) %>%
  ungroup()

# Step 2: Pivot to long format for label + probability pairs
emotion_long <- merged_df %>%
  select(file_id, normalized_position,
         label_0, label_0_prob,
         label_1, label_1_prob,
         label_2, label_2_prob) %>%
  pivot_longer(
    cols = starts_with("label_"),
    names_to = c("label_rank", ".value"),
    names_pattern = "label_(.)_(.+)"
  )

# Step 3: Plot emotion probabilities over normalized position
ggplot(emotion_long, aes(x = normalized_position, y = prob, color = label)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~ label, scales = "free_y") +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Emotion Probability Trends Over Transcript Timeline"
  ) +
  theme_minimal()













library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Normalize position
merged_df <- merged_df %>%
  group_by(file_id) %>%
  mutate(normalized_position = (rank(sentence_id) - 1) / (n() - 1)) %>%
  ungroup()

# Step 2: Reshape softmax emotions to long format
emotion_long <- merged_df %>%
  select(file_id, normalized_position,
         label_0, label_0_prob,
         label_1, label_1_prob,
         label_2, label_2_prob) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("label_rank", ".value"),
    names_pattern = "label_([0-2])_(.*)"
  )

# Step 3: Plot
ggplot(emotion_long, aes(x = normalized_position, y = prob, color = label)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", span = 0.3) +
  facet_wrap(~ label, scales = "free_y") +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Emotion Probability Trends Over Transcript Timeline"
  ) +
  theme_minimal()












library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Normalize position
merged_df <- merged_df %>%
  group_by(file_id) %>%
  mutate(normalized_position = (rank(sentence_id) - 1) / (n() - 1)) %>%
  ungroup()

# Step 2: Rename for pivot
emotion_df <- merged_df %>%
  rename(
    emotion_0 = label_0,
    prob_0 = label_0_prob,
    emotion_1 = label_1,
    prob_1 = label_1_prob,
    emotion_2 = label_2,
    prob_2 = label_2_prob
  )

# Step 3: Pivot and keep normalized_position
emotion_long <- emotion_df %>%
  select(file_id, normalized_position,
         emotion_0, prob_0,
         emotion_1, prob_1,
         emotion_2, prob_2) %>%
  pivot_longer(
    cols = starts_with("emotion") | starts_with("prob"),
    names_to = c(".value", "rank"),
    names_pattern = "(emotion|prob)_(\\d)"
  ) %>%
  filter(!is.na(emotion), !is.na(prob))  # Remove missing values

# Optional: Filter to emotions with at least N observations
emotion_summary <- emotion_long %>%
  group_by(emotion) %>%
  filter(n() >= 10) %>%  # Drop rare labels
  ungroup() %>%
  group_by(normalized_position, emotion) %>%
  summarise(mean_prob = mean(prob), .groups = "drop")

ggplot(emotion_summary, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.2) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Average Emotion Probability",
    title = "Emotion Probability Trends Across All Conversations"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "right"
  )

