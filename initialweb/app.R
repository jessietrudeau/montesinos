



# Load libraries
library(shiny)
library(tidyverse)
library(visNetwork)
library(RColorBrewer)

# Define UI
ui <- fluidPage(
  titlePanel("Transcript-Topic-Speaker Network"),
  tabsetPanel(
    tabPanel("Full Network", visNetworkOutput("network", height = "700px")),
    tabPanel("Speaker-Topic Network",
             visNetworkOutput("speaker_topic_network", height = "700px"),
             br(),
             uiOutput("type_legend")),
    tabPanel("Speaker Co-Appearance Network", visNetworkOutput("speaker_co_network", height = "700px"))
  )
)

# Define Server
server <- function(input, output, session) {
  # Step 1: Load Data
  descriptions       <- read_csv("../data/Descriptions.csv")
  speakers_df        <- read_csv("../data/speakers per transcript.csv")
  topic_descriptions <- read_csv("../data/Topic Descriptions.csv")
  actor_descriptions <- read_csv("../data/Actors.csv") %>%
    mutate(Type = str_trim(Type),
           Type = case_when(
             str_to_lower(Type) == "illict"      ~ "Illicit",
             str_to_lower(Type) == "bereaucrat"  ~ "Bureaucrat",
             TRUE ~ Type
           ))
  actor_descriptions %>%
    count(Type, sort = TRUE)
  
  
  # Step 2: Reshape Topics
  long_topics <- descriptions %>%
    select(n, starts_with("topic_")) %>%
    pivot_longer(cols = starts_with("topic_"), names_to = "topic", values_to = "included") %>%
    filter(included == "x") %>%
    mutate(n = as.character(n), topic = str_remove(topic, "^topic_"))
  
  # Step 3: Reshape Speakers
  speaker_long <- speakers_df %>%
    pivot_longer(cols = -n, names_to = "speaker_col", values_to = "speaker") %>%
    filter(!is.na(speaker), speaker != "") %>%
    mutate(n = as.character(n), speaker = str_trim(speaker))
  
  # Step 4: Create Edges
  edges_topic   <- long_topics %>% select(from = n, to = topic)
  edges_speaker <- speaker_long %>% select(from = speaker, to = n)
  all_edges     <- bind_rows(edges_topic, edges_speaker)
  
  # Step 4B: Speaker ↔ Topic Edges
  edges_speaker_topic <- speaker_long %>%
    inner_join(long_topics, by = "n") %>%
    select(from = speaker, to = topic) %>%
    distinct()
  
  # Step 4C: Speaker Co-Appearance Edges
  speaker_pairs <- speaker_long %>%
    select(n, speaker) %>%
    distinct() %>%
    group_by(n) %>%
    filter(n() > 1) %>%
    summarise(pairs = list(combn(speaker, 2, simplify = FALSE)), .groups = "drop") %>%
    unnest(pairs) %>%
    mutate(from = map_chr(pairs, 1), to = map_chr(pairs, 2)) %>%
    select(from, to) %>%
    filter(from != to)
  
  edges_speaker_co <- speaker_pairs %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop") %>%
    mutate(color = "blue", width = 1)
  
  # Step 5: Create Nodes
  transcript_nodes <- edges_topic %>%
    select(id = from) %>% distinct() %>%
    left_join(descriptions %>% select(n, summary) %>% mutate(n = as.character(n)), by = c("id" = "n")) %>%
    mutate(group = "Transcript", title = str_replace_all(summary, "\n", "<br>")) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  topic_nodes <- edges_topic %>%
    select(id = to) %>% distinct() %>%
    left_join(topic_descriptions %>%
                rename(topic = topics, description = descriptions) %>%
                mutate(topic = str_remove(topic, "^topic_")),
              by = c("id" = "topic")) %>%
    mutate(group = "Topic", title = str_replace_all(description, "\n", "<br>")) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  nodes_speaker <- edges_speaker %>%
    select(id = from) %>% distinct() %>%
    mutate(id = str_trim(id), id_lower = str_to_lower(id)) %>%
    left_join(actor_descriptions %>%
                mutate(speaker_std = str_trim(speaker_std),
                       speaker_std_lower = str_to_lower(speaker_std)),
              by = c("id_lower" = "speaker_std_lower")) %>%
    mutate(group = "Speaker", title = str_replace_all(Position, "\n", "<br>")) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  all_nodes <- bind_rows(transcript_nodes, topic_nodes, nodes_speaker)
  
  # Step 5B: Custom color mapping for Types
  type_colors <- c(
    "Illicit"      = "#e41a1c",  # red
    "Security"     = "#377eb8",  # blue
    "Businessperson"       = "#4daf4a",  # green
    "Bureaucrat"   = "#984ea3",  # purple
    "Congress" = "#ff7f00",  # orange
    "Elected Official"   = "#ffff33",  # yellow
    "Foreign"        = "#a65628",  # brown
    "Judiciary"     = "#f781bf",  # pink
    "Foreign"      = "#999999",  # gray
    "Media"        = "#66c2a5"   # teal
  )
  
  nodes_speaker_st <- speaker_long %>%
    select(id = speaker) %>% distinct() %>%
    mutate(id = str_trim(id), id_lower = str_to_lower(id)) %>%
    left_join(actor_descriptions %>%
                mutate(speaker_std = str_trim(speaker_std),
                       speaker_std_lower = str_to_lower(speaker_std),
                       Type = str_trim(Type),
                       Position = coalesce(Position, "No info")),
              by = c("id_lower" = "speaker_std_lower")) %>%
    mutate(group = "Speaker",
           color = type_colors[Type],
           color = ifelse(is.na(color), "#cccccc", color),
           title = paste0("Speaker: ", id, "<br>Type: ", Type, "<br>Position: ", Position)) %>%
    select(id, group, title, color) %>%
    distinct(id, .keep_all = TRUE)
  
  unmatched_speakers <- nodes_speaker_st %>%
    filter(is.na(color)) %>%
    pull(id) %>%
    unique()
  
  cat("Unmatched Speakers (no Type):\n")
  print(unmatched_speakers)
  
  
  nodes_topic_st <- long_topics %>%
    select(id = topic) %>% distinct() %>%
    left_join(topic_descriptions %>%
                rename(topic = topics, description = descriptions) %>%
                mutate(topic = str_remove(topic, "^topic_")),
              by = c("id" = "topic")) %>%
    mutate(group = "Topic", title = str_replace_all(description, "\n", "<br>")) %>%
    select(id, group, title) %>%
    distinct(id, .keep_all = TRUE)
  
  nodes_st <- bind_rows(nodes_speaker_st, nodes_topic_st)
  
  # Step 5C: Speaker Co-Appearance Network
  node_degrees <- edges_speaker_co %>%
    select(from, to) %>%
    pivot_longer(cols = everything(), values_to = "id") %>%
    count(id, name = "value")
  
  nodes_speaker_co <- node_degrees %>%
    mutate(id = str_trim(id), id_lower = str_to_lower(id)) %>%
    left_join(actor_descriptions %>%
                mutate(speaker_std = str_trim(speaker_std),
                       speaker_std_lower = str_to_lower(speaker_std)),
              by = c("id_lower" = "speaker_std_lower")) %>%
    mutate(group = "Speaker",
           title = str_replace_all(Position, "\n", "<br>"),
           color = "red") %>%
    select(id, group, title, value, color) %>%
    distinct(id, .keep_all = TRUE)
  
  # Step 6: Render Networks
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
  
  output$speaker_topic_network <- renderVisNetwork({
    visNetwork(nodes_st, edges_speaker_topic) %>%
      visNodes(shape = "dot", size = 10) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  output$speaker_co_network <- renderVisNetwork({
    visNetwork(nodes_speaker_co, edges_speaker_co) %>%
      visNodes(shape = "dot") %>%
      visGroups(groupname = "Speaker", color = list(background = "red")) %>%
      visEdges(arrows = "none", color = list(color = "blue")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42)
  })
  
  # Step 7: Legend
  output$type_legend <- renderUI({
    legend_items <- map2_chr(names(type_colors), type_colors, function(type, color) {
      paste0(
        "<div style='display: inline-block; margin-right: 15px; margin-bottom: 4px;'>
           <span style='display: inline-block; width: 12px; height: 12px; background-color:", color, "; border: 1px solid #333; border-radius: 50%; margin-right: 5px;'></span>",
        "<span>", type, "</span>
         </div>"
      )
    })
    HTML(paste("<b>Legend – Speaker Types:</b><br>", paste(legend_items, collapse = "")))
  })
}

# Run the app
shinyApp(ui = ui, server = server)








