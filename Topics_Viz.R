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




















