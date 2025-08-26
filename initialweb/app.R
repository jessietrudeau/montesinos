# Load libraries
library(shiny)
library(tidyverse)
library(visNetwork)
library(RColorBrewer)
library(fs)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Fixed-width visNetwork tooltip with clean word wrapping */
      .vis-tooltip {
        width: 420px !important;
        min-width: 420px !important;
        max-width: 420px !important;
        padding: 8px 10px;
      }
      .vis-tooltip, .vis-tooltip * {
        white-space: normal !important;        /* wrap on spaces */
        word-break: keep-all !important;       /* avoid mid-word breaks */
        overflow-wrap: break-word !important;  /* break only super long tokens */
        hyphens: manual !important;            /* only break at explicit hyphens */
        line-height: 1.35;
      }
    "))
  ),
  titlePanel("Transcript-Topic-Speaker Network"),
  tabsetPanel(
    tabPanel(
      "Speaker-Topic Network",
      fluidRow(
        column(width = 12, visNetworkOutput("speaker_topic_network", height = "700px"))
      ),
      fluidRow(
        column(width = 12, br(), uiOutput("type_legend"))
      )
    ),
    tabPanel(
      "Speaker Co-Appearance Network",
      visNetworkOutput("speaker_co_network", height = "700px")
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output, session) {
  
  # ---------- Local image mounting (no www/, image is in repo's images/ folder) ----------
  # We search common relative locations and mount whichever exists to the URL prefix "/images"
  candidates <- c("images", "../images", "../../images")
  img_dir <- NULL
  for (d in candidates) {
    if (fs::dir_exists(d) && fs::file_exists(fs::path(d, "montesinos.png"))) {
      img_dir <- d
      break
    }
  }
  has_img <- !is.null(img_dir)
  if (has_img) {
    # Mount the found folder at the '/images' URL prefix
    shiny::addResourcePath("images", normalizePath(img_dir))
    montesinos_image <- "/images/montesinos.png"
  } else {
    montesinos_image <- NULL  # will fall back to a dot if not found
    warning("montesinos.png not found under images/, ../images/, or ../../images/")
  }
  
  # ---------- Helpers ----------
  clean_desc <- function(x) {
    # Remove soft hyphen (U+00AD), zero-width space/joins (U+200B/C/D), word joiner (U+2060)
    x <- stringr::str_replace_all(x, "[\\u00AD\\u200B\\u200C\\u200D\\u2060]", "")
    x <- stringr::str_replace_all(x, "[ \t]+", " ")
    stringr::str_squish(x)
  }
  wrap_html_fixed <- function(x) {
    x <- ifelse(is.na(x) | trimws(x) == "", "No description", x)
    x <- clean_desc(x)
    stringr::str_replace_all(x, "\n+", "<br>")
  }
  
  # -----------------------------
  # Step 0: Speaker frequency across all transcripts
  # -----------------------------
  transcript_files <- dir_ls(
    "../data/modified_data/finalized_data",
    regexp = "\\.(csv|tsv)$",
    recurse = TRUE
  )
  
  speaker_freq_all <- map_dfr(transcript_files, function(path) {
    ext <- tools::file_ext(path)
    df <- if (ext == "csv") read_csv(path, col_types = cols()) else read_tsv(path, col_types = cols())
    if (!("speaker_std" %in% names(df))) return(tibble(speaker_std = character(), n = character()))
    transcript_id <- basename(path)
    df %>%
      filter(!is.na(speaker_std), speaker_std != "") %>%
      distinct(speaker_std) %>%
      mutate(n = transcript_id)
  })
  
  speaker_frequency <- speaker_freq_all %>%
    distinct(speaker_std, n) %>%
    count(speaker_std, name = "conversation_count")
  
  # -----------------------------
  # Step 1: Load Data
  # -----------------------------
  descriptions       <- read_csv("../data/Updated Inventory & Descriptions/Descriptions.csv")
  speakers_df        <- read_csv("../data/Updated Inventory & Descriptions/speakers per transcript.csv")
  topic_descriptions <- read_csv("../data/Updated Inventory & Descriptions/Topic Descriptions.csv")
  actor_descriptions_raw <- read_csv("../data/Updated Inventory & Descriptions/Actors.csv")
  
  actor_descriptions <- actor_descriptions_raw %>%
    mutate(
      Type = str_trim(Type),
      Type = str_to_title(Type),
      Type = dplyr::case_when(
        Type %in% c("Illict", "Illicit") ~ "Illicit",
        Type == "Bereaucrat"             ~ "Bureaucrat",
        Type == "Elected official"       ~ "Elected Official",
        Type == "Business"               ~ "Businessperson",
        is.na(Type) | Type == ""         ~ "Unknown",
        TRUE                             ~ Type
      ),
      name = dplyr::coalesce(
        actor_descriptions_raw$name,
        actor_descriptions_raw$Name,
        actor_descriptions_raw$`...1`,
        actor_descriptions_raw$speaker_std
      )
    )
  
  # -----------------------------
  # Step 2: Reshape Topics
  # -----------------------------
  long_topics <- descriptions %>%
    select(n, starts_with("topic_")) %>%
    pivot_longer(cols = starts_with("topic_"), names_to = "topic", values_to = "included") %>%
    filter(included == "x") %>%
    mutate(n = as.character(n), topic = str_remove(topic, "^topic_"))
  
  # -----------------------------
  # Step 3: Reshape Speakers
  # -----------------------------
  speaker_long <- speakers_df %>%
    pivot_longer(cols = -n, names_to = "speaker_col", values_to = "speaker") %>%
    filter(!is.na(speaker), speaker != "") %>%
    mutate(n = as.character(n), speaker = str_trim(speaker))
  
  # -----------------------------
  # Step 4: Speaker ↔ Topic Edges
  # -----------------------------
  edges_speaker_topic <- speaker_long %>%
    inner_join(long_topics, by = "n") %>%
    mutate(speaker_std = str_to_lower(speaker)) %>%
    distinct(speaker_std, topic) %>%
    left_join(speaker_frequency %>% mutate(speaker_std = str_to_lower(speaker_std)), by = "speaker_std") %>%
    transmute(
      from  = speaker_std,
      to    = topic,
      width = pmax(1, log1p(coalesce(conversation_count, 0)))
    )
  
  # -----------------------------
  # Step 4B: Faint Speaker–Speaker edges inside ST view (context only)
  # -----------------------------
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
    mutate(edge_id = paste(pmin(from, to), pmax(from, to), sep = "~~")) %>%
    distinct(edge_id, .keep_all = TRUE) %>%
    select(-edge_id) %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop") %>%
    mutate(
      color = "#bbbbbb",
      width = pmax(1, log1p(weight) / 2)
    )
  
  # -----------------------------
  # Step 5: Speaker Co-Appearance Network (standalone)
  # -----------------------------
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
    mutate(from = str_to_lower(from), to = str_to_lower(to)) %>%
    mutate(edge_id = paste(pmin(from, to), pmax(from, to), sep = "~~")) %>%
    distinct(edge_id, .keep_all = TRUE) %>%
    select(-edge_id) %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop") %>%
    mutate(width = pmax(1, log1p(weight)))
  
  # -----------------------------
  # Step 6: Define Colors
  # -----------------------------
  type_colors <- c(
    "Congress"         = "#BDB2FF",
    "Security"         = "#A0C4FF",
    "Bureaucrat"       = "#CAFFBF",
    "Judiciary"        = "#FDFFB6",
    "Foreign"          = "#FFD6A5",
    "Media"            = "#FFADAD",
    "Illicit"          = "#FFC6FF",
    "Elected Official" = "#9BF6FF",
    "Businessperson"   = "#4daf4a",
    "Unknown"          = "grey"
  )
  
  # -----------------------------
  # Step 7: Build Nodes (Speakers & Topics)
  # -----------------------------
  nodes_speaker_base <- speaker_long %>%
    transmute(id = str_to_lower(str_trim(speaker))) %>%
    distinct()
  
  nodes_speaker_st <- nodes_speaker_base %>%
    left_join(
      actor_descriptions %>%
        mutate(
          speaker_std = str_trim(speaker_std),
          speaker_std_lower = str_to_lower(speaker_std),
          Position = coalesce(Position, "No info"),
          Type = if_else(is.na(Type) | Type == "", "Unknown", Type)
        ),
      by = c("id" = "speaker_std_lower")
    ) %>%
    left_join(
      speaker_frequency %>% mutate(speaker_std = str_to_lower(speaker_std)),
      by = c("id" = "speaker_std")
    ) %>%
    mutate(
      group = "Speaker",
      color = type_colors[Type],
      color = ifelse(is.na(color), type_colors[["Unknown"]], color),
      label = "",
      title = paste0(
        "<b>", coalesce(name, id), "</b><br>",
        "Standardized ID: ", id, "<br>",
        "Type: ", coalesce(Type, "Unknown"), "<br>",
        "Position: ", coalesce(Position, "No info"), "<br>",
        "Transcripts: ", coalesce(as.character(conversation_count), "0")
      )
    ) %>%
    select(id, group, title, color, label) %>%
    distinct(id, .keep_all = TRUE)
  
  # ---- Topic nodes with fixed-width, clean word-wrapped tooltips ----
  nodes_topic_st <- long_topics %>%
    transmute(id = topic) %>%
    distinct() %>%
    left_join(
      topic_descriptions %>%
        rename(topic = topics, description = descriptions) %>%
        mutate(topic = str_remove(topic, "^topic_")),
      by = c("id" = "topic")
    ) %>%
    mutate(
      group = "Topic",
      label = "",
      desc_html = wrap_html_fixed(description),
      title = paste0(
        "<div class='tooltip-body' style='width:420px'>",
        "<b>", stringr::str_to_title(stringr::str_replace_all(id, "_", " ")), "</b><br>",
        desc_html,
        "</div>"
      ),
      value = 300,
      color = "maroon"
    ) %>%
    select(id, group, title, value, color, label) %>%
    distinct(id, .keep_all = TRUE)
  
  # -----------------------------
  # Step 8: Compute true graph degree for speakers (for sizing)
  # -----------------------------
  edges_all_for_degree <- bind_rows(
    edges_speaker_topic %>% mutate(edge_type = "ST"),
    edges_speaker_co    %>% mutate(edge_type = "SS")
  ) %>%
    select(from, to, edge_type)
  
  neighbors_df <- edges_all_for_degree %>%
    select(from, to) %>%
    bind_rows(edges_all_for_degree %>% transmute(from = to, to = from)) %>%
    distinct(from, to) %>%
    group_by(from) %>%
    summarise(degree = n_distinct(to), .groups = "drop")
  
  nodes_speaker_st <- nodes_speaker_st %>%
    left_join(neighbors_df, by = c("id" = "from")) %>%
    mutate(degree = coalesce(degree, 1L)) %>%
    {
      rng <- range(.$degree, na.rm = TRUE)
      dmin <- rng[1]; dmax <- rng[2]
      if (is.finite(dmin) && is.finite(dmax) && dmax > dmin) {
        mutate(., value = 20 + (degree - dmin) * (120 - 20) / (dmax - dmin))
      } else {
        mutate(., value = 40)
      }
    }
  
  # ---- Swap MONTESINOS node to an image (served locally via /images) ----
  nodes_speaker_st <- nodes_speaker_st %>%
    mutate(
      shape = if_else(id == "montesinos" & has_img, "circularImage", "dot"),
      image = if_else(id == "montesinos" & has_img, montesinos_image, NA_character_),
      size  = if_else(id == "montesinos" & has_img, pmax(30, as.numeric(value)), NA_real_),
      borderWidth = if_else(id == "montesinos" & has_img, 0, NA_real_)
    )
  
  # Merge all nodes (keep image-related columns)
  nodes_st <- bind_rows(
    nodes_speaker_st %>% select(id, group, title, color, label, value, shape, image, size, borderWidth),
    nodes_topic_st     %>% mutate(shape = "dot") %>% select(id, group, title, color, label, value, shape)
  ) %>% distinct(id, .keep_all = TRUE)
  
  # -----------------------------
  # Step 9: Speaker-Topic Network (with faint SS edges)
  # -----------------------------
  output$speaker_topic_network <- renderVisNetwork({
    visNetwork(
      nodes_st,
      bind_rows(
        edges_speaker_topic %>% mutate(color = NA_character_),
        edges_placeholder
      )
    ) %>%
      visNodes(
        shape = "dot",
        shapeProperties = list(
          useImageSize = FALSE,         # size images by node$size, not raw pixels
          useBorderWithImage = TRUE
        )
      ) %>%
      visEdges(arrows = "none", color = list(color = "grey")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visLayout(randomSeed = 42) %>%
      visEvents(stabilizationIterationsDone = "function () {
        this.setOptions({ physics: false });
      }")
  })
  
  # -----------------------------
  # Step 10: Co-Appearance Network (speakers only)
  # -----------------------------
  output$speaker_co_network <- renderVisNetwork({
    visNetwork(
      nodes_speaker_st %>% select(id, group, title, color, label, value, shape, image, size, borderWidth),
      edges_speaker_co
    ) %>%
      visNodes(
        shape = "dot",
        shapeProperties = list(
          useImageSize = FALSE,
          useBorderWithImage = TRUE
        )
      ) %>%
      visEdges(arrows = "none", color = list(color = "black")) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based", stabilization = TRUE) %>%
      visLayout(randomSeed = 42) %>%
      visEvents(stabilizationIterationsDone = "function () {
        this.setOptions({ physics: false });
      }")
  })
  
  # -----------------------------
  # Step 11: Legend
  # -----------------------------
  output$type_legend <- renderUI({
    show_colors <- type_colors
    legend_items <- purrr::map2_chr(names(show_colors), show_colors, function(type, color) {
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

# -----------------------------
# Run the app
# -----------------------------
shinyApp(ui = ui, server = server)






