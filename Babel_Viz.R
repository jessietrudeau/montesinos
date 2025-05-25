
library(dplyr)


res<-"data/babel_batches/results_1.csv"
soft<-"data/babel_batches/softmax_1.csv"


results<-read.csv(res)
softmax<-read.csv(soft)

str(softmax)
str(results)

merged_df<-full_join(results, softmax, by = "sentence_id")

res_2<-"data/babel_batches/results_2.csv"
soft_2<-"data/babel_batches/softmax_2.csv"

results_2<-read.csv(res_2)
softmax_2<-read.csv(soft_2)

merged_df_2<-full_join(results_2, softmax_2, by = "sentence_id")

# Merge the two joined datasets into one
combined_df <- bind_rows(merged_df, merged_df_2)


#---------------------------------Overall Trends-------------------------------------


library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Normalize position
combined_df <- combined_df %>%
  group_by(file_id) %>%
  mutate(normalized_position = (rank(sentence_id) - 1) / (n() - 1)) %>%
  ungroup()

# Step 2: Rename for pivot
emotion_df <- combined_df %>%
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


#---------------------Probability Per Conversation----------------------------


library(dplyr)
library(ggplot2)

# Step 1: Aggregate and clamp probabilities between 0 and 1
emotion_summary <- emotion_long %>%
  group_by(file_id, normalized_position, emotion) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(mean_prob = pmin(pmax(mean_prob, 0), 1))

# Step 2: Get all file_ids in the combined dataset
all_files <- combined_df %>%
  distinct(file_id) %>%
  pull(file_id)

# Step 3: Filter the emotion summary for those file_ids
emotion_subset <- emotion_summary %>%
  filter(file_id %in% all_files)

# Step 4: Smoothed plot of all conversations
ggplot(emotion_subset, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, size = 1) +
  facet_wrap(~ file_id, scales = "free_y") +  # allow y-scale to vary across facets
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Smoothed Emotion Trends Across All Conversations"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 7)
  ) +
  scale_color_brewer(palette = "Dark2")


#----------------------------By Topic----------------------------


# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Normalize sentence position within each file
merged_df <- combined_df %>%
  group_by(file_id) %>%
  mutate(normalized_position = (rank(sentence_id) - 1) / (n() - 1)) %>%
  ungroup()

# Step 2: Rename emotion labels and probabilities for pivoting
emotion_df <- combined_df %>%
  rename(
    emotion_0 = label_0,
    prob_0 = label_0_prob,
    emotion_1 = label_1,
    prob_1 = label_1_prob,
    emotion_2 = label_2,
    prob_2 = label_2_prob
  )

# Step 2.5: Clamp raw probability values BEFORE reshaping
emotion_df <- emotion_df %>%
  mutate(
    prob_0 = pmin(pmax(prob_0, 0), 1),
    prob_1 = pmin(pmax(prob_1, 0), 1),
    prob_2 = pmin(pmax(prob_2, 0), 1)
  )

# Step 3: Reshape to long format
emotion_long <- emotion_df %>%
  select(file_id, topics, normalized_position,
         emotion_0, prob_0,
         emotion_1, prob_1,
         emotion_2, prob_2) %>%
  pivot_longer(
    cols = starts_with("emotion") | starts_with("prob"),
    names_to = c(".value", "rank"),
    names_pattern = "(emotion|prob)_(\\d)"
  ) %>%
  filter(!is.na(emotion), !is.na(prob))  # Drop NAs

# Step 4: Aggregate emotion probabilities by topic and position
emotion_topic_summary <- emotion_long %>%
  group_by(topics, normalized_position, emotion) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE), .groups = "drop")

# Step 4.5: Ensure emotion is a factor with all possible levels
emotion_topic_summary <- emotion_topic_summary %>%
  mutate(emotion = factor(emotion, levels = c(
    "Anger", "Disgust", "Enthusiasm", "Fear", "Hope",
    "Joy", "None of Them", "Pride", "Sadness"
  )))

# Clean up topic strings and ensure consistent factor levels
emotion_topic_summary <- emotion_topic_summary %>%
  mutate(
    topics = stringr::str_trim(topics),           # Remove leading/trailing spaces
    topics = forcats::fct_infreq(topics)          # Make topics a factor by frequency
  )

# Step 5: Plot emotion trends by topic
ggplot(emotion_topic_summary, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1) +
  facet_wrap(~ topics, scales = "free_y") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Emotion Trends Across All Topics"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  ) +
  scale_color_brewer(palette = "Dark2")































#-------------------------------Sample for Practice----------------------------------


# Sample 2000 rows (without replacement)
set.seed(123)  # Optional: for reproducibility
sample_df <- merged_df_1 %>% 
  sample_n(2000)

# Write to CSV
write.csv(sample_df, "data/babel_batches/sample_df.csv", row.names = FALSE)


#---------------------------------Overall Trends-------------------------------------


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


#---------------------Probability Per Conversation----------------------------


library(dplyr)
library(ggplot2)

# Step 1: Aggregate and clamp probabilities between 0 and 1
emotion_summary <- emotion_long %>%
  group_by(file_id, normalized_position, emotion) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(mean_prob = pmin(pmax(mean_prob, 0), 1))

# Step 2: Get first 20 conversations
first_20_files <- merged_df %>%
  distinct(file_id) %>%
  slice_head(n = 20) %>%
  pull(file_id)

# Step 3: Filter for those conversations
emotion_subset <- emotion_summary %>%
  filter(file_id %in% first_20_files)

# Step 4: Smoothed plot
ggplot(emotion_subset, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, size = 1) +
  facet_wrap(~ file_id, ncol = 5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Smoothed Emotion Trends Over the First 20 Conversations"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  ) +
  scale_color_brewer(palette = "Dark2")  # Optional: colorblind-friendly

library(dplyr)
library(ggplot2)

# Step 1: Aggregate and clamp emotion probabilities
emotion_summary <- emotion_long %>%
  group_by(file_id, normalized_position, emotion) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(mean_prob = pmin(pmax(mean_prob, 0), 1))

# Step 2: Get LAST 20 file_id values
last_20_files <- merged_df %>%
  distinct(file_id) %>%
  arrange(desc(file_id)) %>%
  slice_head(n = 20) %>%
  pull(file_id)

# Step 3: Filter emotion_summary for those file_ids
emotion_subset <- emotion_summary %>%
  filter(file_id %in% last_20_files)

# Optional: Force emotion factor levels to retain all in legend
emotion_subset$emotion <- factor(
  emotion_subset$emotion,
  levels = c("Anger", "Disgust", "Enthusiasm", "Fear", "Hope",
             "Joy", "None of Them", "Pride", "Sadness")
)

# Step 4: Plot smoothed emotion trends
ggplot(emotion_subset, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, size = 1) +
  facet_wrap(~ file_id, ncol = 5) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Smoothed Emotion Trends Over the Last 20 Conversations"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  ) +
  scale_color_brewer(palette = "Dark2")


#----------------------------By Topic----------------------------


# How many rows originally belong to the topic
#nrow(merged_df %>% filter(topics == "ecuador, media"))

# How many survived in long format
#emotion_long %>% filter(topics == "ecuador, media") %>% nrow()

#emotion_topic_summary %>%
#  filter(topics == "ecuador, media")

# Step 4: Aggregate emotion probabilities by topic and position
#emotion_topic_summary <- emotion_long %>%
#  group_by(topics, normalized_position, emotion) %>%
#  summarise(
#    mean_prob = mean(prob, na.rm = TRUE),
#    .groups = "drop"
#  ) %>%
#  filter(!is.na(mean_prob))  # Drop any remaining NA values explicitly

#emotion_topic_summary <- emotion_topic_summary %>%
#  mutate(emotion = factor(emotion, levels = c(
#    "Anger", "Disgust", "Enthusiasm", "Fear", "Hope",
#    "Joy", "None of Them", "Pride", "Sadness"
#  )))

# Check min/max values for each probability column
#summary(merged_df$label_0_prob)
#summary(merged_df$label_1_prob)
#summary(merged_df$label_2_prob)

# Count how many invalid values exist in each column
#sum(merged_df$label_0_prob < 0 | merged_df$label_0_prob > 1, na.rm = TRUE)
#sum(merged_df$label_1_prob < 0 | merged_df$label_1_prob > 1, na.rm = TRUE)
#sum(merged_df$label_2_prob < 0 | merged_df$label_2_prob > 1, na.rm = TRUE)


# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Normalize sentence position within each file
merged_df <- merged_df %>%
  group_by(file_id) %>%
  mutate(normalized_position = (rank(sentence_id) - 1) / (n() - 1)) %>%
  ungroup()

# Step 2: Rename emotion labels and probabilities for pivoting
emotion_df <- merged_df %>%
  rename(
    emotion_0 = label_0,
    prob_0 = label_0_prob,
    emotion_1 = label_1,
    prob_1 = label_1_prob,
    emotion_2 = label_2,
    prob_2 = label_2_prob
  )

# Step 2.5: Clamp raw probability values BEFORE reshaping
emotion_df <- emotion_df %>%
  mutate(
    prob_0 = pmin(pmax(prob_0, 0), 1),
    prob_1 = pmin(pmax(prob_1, 0), 1),
    prob_2 = pmin(pmax(prob_2, 0), 1)
  )

# Step 3: Reshape to long format
emotion_long <- emotion_df %>%
  select(file_id, topics, normalized_position,
         emotion_0, prob_0,
         emotion_1, prob_1,
         emotion_2, prob_2) %>%
  pivot_longer(
    cols = starts_with("emotion") | starts_with("prob"),
    names_to = c(".value", "rank"),
    names_pattern = "(emotion|prob)_(\\d)"
  ) %>%
  filter(!is.na(emotion), !is.na(prob))  # Drop NAs

# Step 4: Aggregate emotion probabilities by topic and position
emotion_topic_summary <- emotion_long %>%
  group_by(topics, normalized_position, emotion) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE), .groups = "drop")

# Step 4.5: Ensure emotion is a factor with all possible levels
emotion_topic_summary <- emotion_topic_summary %>%
  mutate(emotion = factor(emotion, levels = c(
    "Anger", "Disgust", "Enthusiasm", "Fear", "Hope",
    "Joy", "None of Them", "Pride", "Sadness"
  )))

# Clean up topic strings and ensure consistent factor levels
emotion_topic_summary <- emotion_topic_summary %>%
  mutate(
    topics = stringr::str_trim(topics),           # Remove leading/trailing spaces
    topics = forcats::fct_infreq(topics)          # Make topics a factor by frequency
  )

# Step 5: Plot emotion trends by topic
ggplot(emotion_topic_summary, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1) +
  facet_wrap(~ topics, scales = "free_y") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Emotion Trends Across All Topics"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  ) +
  scale_color_brewer(palette = "Dark2")

















#-------------------------------By Conversation------------------------------

unique(emotion_long$file_id)  # Does 4 appear here?

emotion_summary %>% filter(file_id == 4)




library(dplyr)
library(ggplot2)

# Step 1: Clamp values between 0 and 1 (sanity check to prevent overshooting)
emotion_summary <- emotion_long %>%
  group_by(file_id, normalized_position, emotion) %>%
  summarise(mean_prob = mean(prob, na.rm = TRUE), .groups = "drop") %>%
  mutate(mean_prob = pmin(pmax(mean_prob, 0), 1))  # Clamp to [0, 1]

# Step 2: Get first 20 file_id values
first_20_files <- merged_df %>%
  distinct(file_id) %>%
  slice_head(n = 20) %>%
  pull(file_id)

# Step 3: Filter emotion_summary
emotion_subset <- emotion_summary %>%
  filter(file_id %in% first_20_files)

# Step 4: Plot with constrained y-axis
ggplot(emotion_subset, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1) +
  facet_wrap(~ file_id) +
  coord_cartesian(ylim = c(0, 1)) +  # Constrain y-axis
  labs(
    x = "Normalized Position in Conversation",
    y = "Average Emotion Probability",
    title = "Emotion Probability Trends for First 20 Conversations"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  )



ggplot(emotion_subset, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_point(alpha = 0.5, size = 1) +  # Add this line
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1) +
  facet_wrap(~ file_id) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Average Emotion Probability",
    title = "Emotion Probability Trends for First 20 Conversations"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  )





# Step 1: Get last 20 file_id values
last_20_files <- merged_df %>%
  distinct(file_id) %>%
  slice_tail(n = 20) %>%
  pull(file_id)

# Step 2: Filter emotion_summary
emotion_subset <- emotion_summary %>%
  filter(file_id %in% last_20_files)

# Step 3: Plot
ggplot(emotion_subset, aes(x = normalized_position, y = mean_prob, color = emotion)) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, size = 1.2) +
  facet_wrap(~ file_id) +
  labs(
    x = "Normalized Position in Conversation",
    y = "Average Emotion Probability",
    title = "Emotion Probability Trends for Last 20 Conversations"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  )





















#-------------------------Probability by Conversation------------------------

first_20_topics <- merged_df %>%
  distinct(topics) %>%
  slice_head(n = 20) %>%
  pull(topics)

emotion_long_topic <- emotion_long %>%
  left_join(
    merged_df %>% select(file_id, sentence_id, topics),
    by = c("file_id", "sentence_id")
  ) %>%
  rename(topic = topics) %>%
  filter(!is.na(topic))

subset_topic_data <- emotion_long_topic %>%
  filter(topic %in% first_20_topics)

ggplot(subset_topic_data, aes(x = normalized_position, y = prob, color = emotion)) +
  geom_line(alpha = 0.2, aes(group = interaction(file_id, emotion))) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1) +
  facet_wrap(~ topic, scales = "free_y") +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Emotion Probability Trends Across First 20 Compound Topics"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 6)
  )






last_20_topics <- merged_df %>%
  distinct(topics) %>%
  slice_tail(n = 20) %>%
  pull(topics)

emotion_long_topic <- emotion_long %>%
  left_join(
    merged_df %>% select(file_id, sentence_id, topics),
    by = c("file_id", "sentence_id")
  ) %>%
  rename(topic = topics) %>%
  filter(!is.na(topic))

subset_topic_data <- emotion_long_topic %>%
  filter(topic %in% last_20_topics)

ggplot(subset_topic_data, aes(x = normalized_position, y = prob, color = emotion)) +
  geom_line(alpha = 0.2, aes(group = interaction(file_id, emotion))) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1) +
  facet_wrap(~ topic, scales = "free_y") +
  labs(
    x = "Normalized Position in Conversation",
    y = "Emotion Probability",
    title = "Emotion Probability Trends Across Last 20 Compound Topics"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 6)
  )
























# Step 1: Normalize position within each conversation (file_id)
merged_df <- merged_df %>%
  group_by(file_id) %>%
  mutate(
    normalized_position = (rank(sentence_id, ties.method = "first") - 1) / (n() - 1)
  ) %>%
  ungroup()

# Step 2: Reshape emotion labels and probabilities to long format
labels_long <- merged_df %>%
  select(file_id, sentence_id, topic, normalized_position,
         label_0, label_1, label_2) %>%
  pivot_longer(
    cols = starts_with("label_"),
    names_to = "rank",
    names_pattern = "label_(\\d)",
    values_to = "emotion"
  )

# Reshape emotion probabilities (numeric)
probs_long <- merged_df %>%
  select(file_id, sentence_id, label_0_prob, label_1_prob, label_2_prob) %>%
  pivot_longer(
    cols = everything(),
    names_to = "name",
    values_to = "prob"
  ) %>%
  mutate(
    rank = str_extract(name, "\\d")  # Extract just the rank digit
  ) %>%
  select(-name)



# Join the two
emotion_long <- labels_long %>%
  left_join(probs_long, by = c("file_id", "sentence_id", "rank"))


# Step 3: Select the first 20 unique topics in the order they appear
first_20_topics <- emotion_long %>%
  filter(!is.na(topic)) %>%
  distinct(topic) %>%
  slice_head(n = 20) %>%
  pull(topic)

# Step 4: Filter the data for those topics and convert to factors
subset_topic_data <- emotion_long %>%
  filter(topic %in% first_20_topics) %>%
  mutate(
    topic = factor(topic, levels = first_20_topics),
    emotion = as.factor(emotion)
  )

# Step 5: Plot emotion probability trends by topic
ggplot(subset_topic_data, aes(x = normalized_position, y = prob, color = emotion)) +
  geom_line(alpha = 0.2, aes(group = interaction(file_id, emotion))) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1) +
  facet_wrap(~ topic, scales = "free_y") +
  labs(
    title = "Emotion Probability Trends by Topic (First 20 Topics)",
    x = "Normalized Position in Conversation",
    y = "Emotion Probability"
  ) +
  theme_minimal()








