




# Load libraries
library(shiny)
library(tidyverse)
library(visNetwork)

# Define UI
ui <- fluidPage(
  titlePanel("Transcript-Topic-Speaker Network"),
  tabsetPanel(
    tabPanel("Full Network", visNetworkOutput("network", height = "700px")),
    tabPanel("Speaker-Topic Network", visNetworkOutput("speaker_topic_network", height = "700px"))
  )
)

# Define Server
server <- function(input, output, session) {
  
  # ------------------------ Step 1: Load Data ------------------------
  descriptions       <- read_csv("../data/Descriptions.csv")
  speakers_df        <- read_csv("../data/speakers per transcript.csv")
  topic_descriptions <- read_csv("../data/Topic Descriptions.csv")
  actor_descriptions <- read_csv("../data/Actors Description.csv")
  
  # ------------------------ Step 2: Reshape Topics ------------------------
  long_topics <- descriptions %>%
    select(n, starts_with("topic_")) %>%
    pivot_longer(
      cols = starts_with("topic_"),
      names_to = "topic",
      values_to = "included"
    ) %>%
    filter(included == "x") %>%
    mutate(
      n     = as.character(n),
      topic = str_remove(topic, "^topic_")
    )
  
  # ------------------------ Step 3: Reshape Speakers ------------------------
  speaker_long <- speakers_df %>%
    pivot_longer(
      cols = -n,
      names_to = "speaker_col",
      values_to = "speaker"
    ) %>%
    filter(!is.na(speaker), speaker != "") %>%
    mutate(
      n       = as.character(n),
      speaker = str_trim(speaker)
    )
  
  # ------------------------ Step 4: Create Edges ------------------------
  edges_topic <- long_topics %>%
    select(from = n, to = topic)
  
  edges_speaker <- speaker_long %>%
    select(from = speaker, to = n)
  
  all_edges <- bind_rows(edges_topic, edges_speaker)
  
  # ------------------------ Step 4B: Speaker ↔ Topic Edges ------------------------
  edges_speaker_topic <- speaker_long %>%
    inner_join(long_topics, by = "n") %>%
    select(from = speaker, to = topic) %>%
    distinct()
  
  # ------------------------ Step 5: Create Nodes ------------------------
  transcript_nodes <- edges_topic %>%
    select(id = from) %>% distinct() %>%
    left_join(
      descriptions %>% select(n, summary) %>% mutate(n = as.character(n)),
      by = c("id" = "n")
    ) %>%
    mutate(
      group = "Transcript",
      title = str_replace_all(summary, "\n", "<br>")
    ) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  topic_nodes <- edges_topic %>%
    select(id = to) %>% distinct() %>%
    left_join(
      topic_descriptions %>%
        rename(topic = topics, description = descriptions) %>%
        mutate(topic = str_remove(topic, "^topic_")),
      by = c("id" = "topic")
    ) %>%
    mutate(
      group = "Topic",
      title = str_replace_all(description, "\n", "<br>")
    ) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  nodes_speaker <- edges_speaker %>%
    select(id = from) %>% distinct() %>%
    mutate(id = str_trim(id), id_lower = str_to_lower(id)) %>%
    left_join(
      actor_descriptions %>%
        mutate(speaker_std = str_trim(speaker_std),
               speaker_std_lower = str_to_lower(speaker_std)),
      by = c("id_lower" = "speaker_std_lower")
    ) %>%
    mutate(
      group = "Speaker",
      title = str_replace_all(description, "\n", "<br>")
    ) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  all_nodes <- bind_rows(transcript_nodes, topic_nodes, nodes_speaker)
  
  # ------------------------ Step 5B: Nodes for Speaker-Topic Network ------------------------
  nodes_speaker_st <- speaker_long %>%
    select(id = speaker) %>%
    distinct() %>%
    mutate(id = str_trim(id), id_lower = str_to_lower(id)) %>%
    left_join(
      actor_descriptions %>%
        mutate(speaker_std = str_trim(speaker_std),
               speaker_std_lower = str_to_lower(speaker_std)),
      by = c("id_lower" = "speaker_std_lower")
    ) %>%
    mutate(
      group = "Speaker",
      title = str_replace_all(description, "\n", "<br>")
    ) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  nodes_topic_st <- long_topics %>%
    select(id = topic) %>%
    distinct() %>%
    left_join(
      topic_descriptions %>%
        rename(topic = topics, description = descriptions) %>%
        mutate(topic = str_remove(topic, "^topic_")),
      by = c("id" = "topic")
    ) %>%
    mutate(
      group = "Topic",
      title = str_replace_all(description, "\n", "<br>")
    ) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  nodes_st <- bind_rows(nodes_speaker_st, nodes_topic_st)
  
  # ------------------------ Step 6: Debug Output ------------------------
  print(paste("Full Network - Nodes:", nrow(all_nodes)))
  print(paste("Full Network - Edges:", nrow(all_edges)))
  print(paste("Speaker-Topic Network - Nodes:", nrow(nodes_st)))
  print(paste("Speaker-Topic Network - Edges:", nrow(edges_speaker_topic)))
  
  # ------------------------ Step 7: Render Full Network ------------------------
  output$network <- renderVisNetwork({
    visNetwork(all_nodes, all_edges) %>%
      visNodes(shape = "dot", size = 10) %>%
      visGroups(groupname = "Transcript", color = list(background = "skyblue")) %>%
      visGroups(groupname = "Topic", color = list(background = "salmon")) %>%
      visGroups(groupname = "Speaker", color = list(background = "lightgreen")) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  # ------------------------ Step 8: Render Speaker-Topic Network ------------------------
  output$speaker_topic_network <- renderVisNetwork({
    visNetwork(nodes_st, edges_speaker_topic) %>%
      visNodes(shape = "dot", size = 10) %>%
      visGroups(groupname = "Speaker", color = list(background = "lightgreen")) %>%
      visGroups(groupname = "Topic", color = list(background = "salmon")) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
}

# Run the app
shinyApp(ui = ui, server = server)










