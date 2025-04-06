# Install and load the required libraries
install.packages("stm")
install.packages(c("geometry", "Rtsne", "rsvd"))
install.packages("reshape2")

library(geometry)
library(Rtsne)
library(rsvd)
library(stm)
library(tidyverse)
library(tidytext)
library(reshape2)


# Convert quanteda dfm to STM format
dfm_stm <- convert(dfm_general, to = "stm")

saveRDS(dfm_stm, file = "dfm_stm.rds")

#-------------------------------------5 Topics


# Define the number of topics (e.g., 5 topics)
num_topics_1 <- 5

# Fit the STM model
stm_model_1 <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = num_topics_1,
  prevalence = ~ speaker,  # Include metadata if available
  max.em.its = 75,
  data = dfm_stm$meta
)

# Inspect top words for each topic
labelTopics(stm_model_1, n=10)

#Save stm_model
saveRDS(stm_model_1, file = "stm_model_1.rds")

stm_model_1 <- readRDS("stm_model_1.rds")

# Plot topic proportions
plot.STM(stm_model_1, type = "labels", labeltype = "frex")

# Extract beta values for terms per topic
td_beta <- tidy(stm_model_1)

# Filter for top 10 terms per topic
top_terms <- td_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Plot using ggplot2
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms by Topic",
       x = "Term",
       y = "Probability (Beta)")








# Get the top FREX and highest probability terms using labelTopics
labels <- labelTopics(stm_model_1, n = 10)

# Combine FREX and Prob terms into a data frame
# We'll use FREX for plotting, but you can adjust if you want to show both
topic_terms <- data.frame(
  topic = rep(1:num_topics_1, each = 10),
  frex = as.vector(t(labels$frex)),
  prob = as.vector(t(labels$prob))
)

# For better ggplot compatibility, use `term_label` to combine both
topic_terms <- topic_terms %>%
  mutate(term_label = paste0(frex, " (", prob, ")"))

# Optional: match with beta values for plotting by weight
td_beta <- tidy(stm_model_1)

# Join to get beta values for plotting FREX terms only
top_terms <- td_beta %>%
  inner_join(topic_terms, by = c("topic", "term" = "frex"))

# Plot using ggplot2 with combined labels
ggplot(top_terms, aes(x = reorder_within(term_label, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top FREX Terms by Topic with Probability Labels",
       x = "Term (FREX with Most Probable Word)",
       y = "Probability (Beta)")














#----------------------------------Topics 10


# Define the number of topics (e.g., 5 topics)
num_topics_2 <- 10

# Fit the STM model
stm_model_2 <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = num_topics_2,
  prevalence = ~ speaker,  # Include metadata if available
  max.em.its = 75,
  data = dfm_stm$meta
)

# Inspect top words for each topic
labelTopics(stm_model_2, n=10)

#Save stm_model
saveRDS(stm_model_2, file = "stm_model_2.rds")

stm_model_2 <- readRDS("stm_model_2.rds")

# Plot topic proportions
plot.STM(stm_model_2, type = "labels", labeltype = "frex")

# Extract beta values for terms per topic
td_beta <- tidy(stm_model_2)

# Filter for top 10 terms per topic
top_terms <- td_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Plot using ggplot2
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms by Topic",
       x = "Term",
       y = "Probability (Beta)")











# Extract top FREX and Prob terms
labels_2 <- labelTopics(stm_model_2, n = 10)

# Create a data frame with FREX and Prob terms
topic_terms_2 <- data.frame(
  topic = rep(1:num_topics_2, each = 10),
  frex = as.vector(t(labels_2$frex)),
  prob = as.vector(t(labels_2$prob))
)

# Add a combined label to show FREX with most probable word
topic_terms_2 <- topic_terms_2 %>%
  mutate(term_label = paste0(frex, " (", prob, ")"))

# Extract beta values from the model
td_beta_2 <- tidy(stm_model_2)

# Join beta values with FREX terms for plotting
top_terms_2 <- td_beta_2 %>%
  inner_join(topic_terms_2, by = c("topic", "term" = "frex"))

# Plot with ggplot2
ggplot(top_terms_2, aes(x = reorder_within(term_label, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top FREX Terms by Topic with Probability Labels (Model 2)",
       x = "FREX Term (Most Probable Term)",
       y = "Probability (Beta)")









#-----------------------------20 Topics


# Define the number of topics (e.g., 5 topics)
num_topics_3 <- 20

# Fit the STM model
stm_model_3 <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = num_topics_3,
  prevalence = ~ speaker,  # Include metadata if available
  max.em.its = 75,
  data = dfm_stm$meta
)

# Inspect top words for each topic
labelTopics(stm_model_3, n=10)

#Save stm_model
saveRDS(stm_model_3, file = "stm_model_3.rds")

stm_model_3 <- readRDS("stm_model_3.rds")

# Plot topic proportions
plot.STM(stm_model_3, type = "labels", labeltype = "frex")

# Extract beta values for terms per topic
td_beta <- tidy(stm_model_3)

# Filter for top 10 terms per topic
top_terms <- td_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Plot using ggplot2
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms by Topic",
       x = "Term",
       y = "Probability (Beta)")














# Extract top FREX and Prob terms
labels_3 <- labelTopics(stm_model_3, n = 10)

# Create a data frame with FREX and Prob terms
topic_terms_3 <- data.frame(
  topic = rep(1:num_topics_3, each = 10),
  frex = as.vector(t(labels_3$frex)),
  prob = as.vector(t(labels_3$prob))
)

# Combine FREX and Prob term for labeling
topic_terms_3 <- topic_terms_3 %>%
  mutate(term_label = paste0(frex, " (", prob, ")"))

# Extract beta values
td_beta_3 <- tidy(stm_model_3)

# Join with FREX terms to include beta values for plotting
top_terms_3 <- td_beta_3 %>%
  inner_join(topic_terms_3, by = c("topic", "term" = "frex"))

# Plot with ggplot2
ggplot(top_terms_3, aes(x = reorder_within(term_label, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top FREX Terms by Topic with Probability Labels (Model 3)",
       x = "FREX Term (Most Probable Term)",
       y = "Probability (Beta)")










#-----------------------------Topics 0


# Define the number of topics (e.g., 5 topics)
num_topics_4 <- 0

# Fit the STM model
stm_model_4 <- stm(
  documents = dfm_stm$documents,
  vocab = dfm_stm$vocab,
  K = num_topics_4,
  prevalence = ~ speaker,  # Include metadata if available
  max.em.its = 75,
  data = dfm_stm$meta
)

# Inspect top words for each topic
labelTopics(stm_model_4, n=10)

#Save stm_model
saveRDS(stm_model_4, file = "stm_model_4.rds")

stm_model_4 <- readRDS("stm_model_4.rds")

# Plot topic proportions
plot.STM(stm_model_4, type = "labels", labeltype = "frex")

# Extract beta values for terms per topic
td_beta <- tidy(stm_model_4)

# Filter for top 10 terms per topic
top_terms <- td_beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup()

# Plot using ggplot2
ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top Terms by Topic",
       x = "Term",
       y = "Probability (Beta)")









# Extract number of topics from model
num_topics_actual <- stm_model_4$settings$dim$K

# Extract top FREX and Prob terms
labels_4 <- labelTopics(stm_model_4, n = 10)

# Create data frame for FREX and Prob terms
topic_terms_4 <- data.frame(
  topic = rep(1:num_topics_actual, each = 10),
  frex = as.vector(t(labels_4$frex)),
  prob = as.vector(t(labels_4$prob))
)

# Combine FREX and Prob term for readable labels
topic_terms_4 <- topic_terms_4 %>%
  mutate(term_label = paste0(frex, " (", prob, ")"))

# Get beta values from STM model
td_beta_4 <- tidy(stm_model_4)

# Join with FREX terms
top_terms_4 <- td_beta_4 %>%
  inner_join(topic_terms_4, by = c("topic", "term" = "frex"))

# Plot with ggplot2
ggplot(top_terms_4, aes(x = reorder_within(term_label, beta, topic), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = paste("Top FREX Terms by Topic with Probabilities (Model 4, K =", num_topics_actual, ")"),
       x = "FREX Term (Most Probable Term)",
       y = "Probability (Beta)")

















