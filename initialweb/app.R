



# Load libraries
library(shiny)
library(tidyverse)
library(visNetwork)
library(RColorBrewer)
library(fs)

# Define UI
ui <- fluidPage(
  titlePanel("Transcript-Topic-Speaker Network"),
  tabsetPanel(
    tabPanel("Speaker-Topic Network",
             fluidRow(
               column(width = 12, visNetworkOutput("speaker_topic_network", height = "700px"))
             ),
             fluidRow(
               column(width = 12, br(), uiOutput("type_legend"))
             )),
    tabPanel("Speaker Co-Appearance Network",
             visNetworkOutput("speaker_co_network", height = "700px"))
  )
)

# Define Server
server <- function(input, output, session) {
  # Step 0: Speaker frequency across all transcripts
  transcript_files <- dir_ls("../data/modified_data/finalized_data", regexp = "\\.(csv|tsv)$", recurse = TRUE)
  
  speaker_freq_all <- map_dfr(transcript_files, function(path) {
    ext <- tools::file_ext(path)
    df <- if (ext == "csv") read_csv(path, col_types = cols()) else read_tsv(path, col_types = cols())
    
    if (!("speaker_std" %in% names(df))) {
      return(tibble(speaker_std = character(), n = character()))
    }
    
    transcript_id <- basename(path)
    
    df %>%
      filter(!is.na(speaker_std), speaker_std != "") %>%
      distinct(speaker_std) %>%
      mutate(n = transcript_id)
  })
  
  speaker_frequency <- speaker_freq_all %>%
    distinct(speaker_std, n) %>%
    count(speaker_std, name = "conversation_count")
  
  # Step 1: Load Data
  descriptions       <- read_csv("../data/Updated Inventory & Descriptions/Descriptions.csv")
  speakers_df        <- read_csv("../data/Updated Inventory & Descriptions/speakers per transcript.csv")
  topic_descriptions <- read_csv("../data/Updated Inventory & Descriptions/Topic Descriptions.csv")
  actor_descriptions <- read_csv("../data/Updated Inventory & Descriptions/Actors.csv") %>%
    mutate(Type = str_trim(Type),
           Type = str_to_title(Type),
           Type = case_when(
             Type %in% c("Illict", "Illicit") ~ "Illicit",
             Type == "Bereaucrat"             ~ "Bureaucrat",
             Type == "Elected official"       ~ "Elected Official",
             Type == "Business"               ~ "Businessperson",
             TRUE                             ~ Type
           ))
  
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
  
  # Step 4: Speaker ↔ Topic Edges with frequency-based width
  edges_speaker_topic <- speaker_long %>%
    inner_join(long_topics, by = "n") %>%
    mutate(speaker_std = str_to_lower(speaker)) %>%
    distinct(speaker_std, topic) %>%
    left_join(speaker_frequency %>% mutate(speaker_std = str_to_lower(speaker_std)), by = "speaker_std") %>%
    transmute(from = speaker_std, to = topic, width = pmax(1, log1p(conversation_count)))
  
  # Step 4B: Speaker Co-Appearance Edges (used as placeholder for shared transcripts)
  speaker_pairs_topic_net <- speaker_long %>%
    select(n, speaker) %>%
    distinct() %>%
    group_by(n) %>%
    filter(n() > 1) %>%
    summarise(pairs = list(combn(str_to_lower(speaker), 2, simplify = FALSE)), .groups = "drop") %>%
    unnest(pairs) %>%
    mutate(from = map_chr(pairs, 1), to = map_chr(pairs, 2)) %>%
    select(from, to) %>%
    filter(from != to)
  
  edges_placeholder <- speaker_pairs_topic_net %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop") %>%
    mutate(color = "black", width = 1)
  
  # Step 5: Speaker Co-Appearance Edges
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
    mutate(width = 1)
  
  # Step 6: Define Colors by Type
  type_colors <- c(
    "Illicit"          = "#e41a1c",
    "Security"         = "#377eb8",
    "Businessperson"   = "#4daf4a",
    "Bureaucrat"       = "#984ea3",
    "Congress"         = "#ff7f00",
    "Elected Official" = "#ffff33",
    "Foreign"          = "#349",
    "Judiciary"        = "#f781bf",
    "Media"            = "#66c2a5"
  )
  
  # Step 7: Speaker-Topic Nodes
  nodes_speaker_st <- speaker_long %>%
    select(speaker) %>% distinct() %>%
    mutate(id = str_to_lower(speaker)) %>%
    mutate(id = str_trim(id), id_lower = str_to_lower(id)) %>%
    left_join(actor_descriptions %>%
                mutate(speaker_std = str_trim(speaker_std),
                       speaker_std_lower = str_to_lower(speaker_std),
                       Type = str_trim(Type),
                       Position = coalesce(Position, "No info")),
              by = c("id_lower" = "speaker_std_lower")) %>%
    left_join(speaker_frequency %>% mutate(speaker_std = str_to_lower(speaker_std)),
              by = c("id" = "speaker_std")) %>%
    mutate(
      group = "Speaker",
      color = type_colors[Type],
      color = ifelse(is.na(color), "#cccccc", color),
      title = paste0(
        "Speaker: ", id, "<br>Type: ", Type, "<br>Position: ", Position,
        "<br>Transcripts: ", conversation_count
      ),
      value = case_when(
        conversation_count > 80 ~ 140,
        conversation_count > 60 ~ 100,
        conversation_count > 40 ~ 80,
        conversation_count > 20 ~ 60,
        TRUE                    ~ 40
      )
    ) %>%
    select(id, group, title, color, value) %>%
    distinct(id, .keep_all = TRUE)
  
  nodes_topic_st <- long_topics %>%
    select(id = topic) %>% distinct() %>%
    left_join(topic_descriptions %>%
                rename(topic = topics, description = descriptions) %>%
                mutate(topic = str_remove(topic, "^topic_")),
              by = c("id" = "topic")) %>%
    mutate(
      group = "Topic",
      title = str_replace_all(description, "\n", "<br>"),
      value = 300,
      color = "maroon"
    ) %>%
    select(id, group, title, value, color) %>%
    distinct(id, .keep_all = TRUE)
  
  nodes_st <- bind_rows(nodes_speaker_st, nodes_topic_st)
  
  # Step 8: Render Speaker-Topic Network with placeholder co-appearance edges
  output$speaker_topic_network <- renderVisNetwork({
    visNetwork(nodes_st, bind_rows(edges_speaker_topic, edges_placeholder)) %>%
      visNodes(shape = "dot") %>%
      visEdges(arrows = "none", color = list(color = "grey")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visLayout(randomSeed = 42) %>%
      visEvents(stabilizationIterationsDone = "function () {
        this.setOptions({ physics: false });
      }")
  })
  
  # Step 9: Render Co-Appearance Network
  output$speaker_co_network <- renderVisNetwork({
    visNetwork(nodes_speaker_st, edges_speaker_co) %>%
      visNodes(shape = "dot") %>%
      visGroups(groupname = "Speaker", color = list(background = "red")) %>%
      visEdges(arrows = "none", color = list(color = "black")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visLayout(randomSeed = 42) %>%
      visEvents(stabilizationIterationsDone = "function () {
        this.setOptions({ physics: false });
      }")
  })
  
  # Step 10: Legend
  output$type_legend <- renderUI({
    legend_items <- map2_chr(names(type_colors), type_colors, function(type, color) {
      paste0(
        "<div style='display: inline-block; margin-right: 15px; margin-bottom: 4px; vertical-align: middle;'>
           <span style='display: inline-block; width: 14px; height: 14px; background-color:", color, "; border: 1px solid #333; border-radius: 50%; margin-right: 6px;'></span>",
        "<span style='font-size: 14px;'>", type, "</span>
         </div>"
      )
    })
    HTML(paste("<b>Legend – Speaker Types:</b><br><div style='margin-top: 5px;'>", paste(legend_items, collapse = ""), "</div>"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

