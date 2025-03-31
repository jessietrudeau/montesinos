# 5 Topics


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


# 10 Topics


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


# 20 Topics


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


# 0 Topics


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


