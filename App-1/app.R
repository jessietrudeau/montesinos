# app.R

library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)

# --- UI Definition ---
ui <- fluidPage(
  titlePanel("Transcript File Word Count Viewer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("fileSlider")  # Dynamic slider
    ),
    mainPanel(
      textOutput("selectedFile"),
      textOutput("wordCount"),
      plotOutput("barPlot")
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  transcript_dir <- "../../data/modified_data/finalized_data"
  
  # List files (CSV and TSV)
  all_files <- list.files(transcript_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)
  
  # Extract filenames (just base names)
  filenames <- basename(all_files)
  
  # --- Count words function ---
  count_words <- function(text) {
    if (is.null(text) || all(is.na(text))) return(0)
    text <- na.omit(text)
    sum(str_count(text, "\\S+"))
  }
  
  # --- Precompute word counts for all files ---
  file_word_data <- reactive({
    result <- lapply(all_files, function(file) {
      df <- tryCatch({
        if (grepl("\\.csv$", file)) read_csv(file, show_col_types = FALSE)
        else read_tsv(file, show_col_types = FALSE)
      }, error = function(e) return(NULL))
      
      wc <- if (!is.null(df) && "speech" %in% names(df)) count_words(df$speech) else NA
      data.frame(File = basename(file), Word_Count = wc)
    })
    
    bind_rows(result)
  })
  
  # --- Dynamically create slider input based on file count ---
  output$fileSlider <- renderUI({
    sliderInput(
      inputId = "fileIndex",
      label = "Select Transcript File (by index)",
      min = 1,
      max = length(filenames),
      value = 1,
      step = 1,
      ticks = FALSE
    )
  })
  
  # --- Display selected file name and word count ---
  output$selectedFile <- renderText({
    req(input$fileIndex)
    paste("Selected File:", filenames[input$fileIndex])
  })
  
  output$wordCount <- renderText({
    req(input$fileIndex)
    wc <- file_word_data()$Word_Count[input$fileIndex]
    paste("Word Count:", wc)
  })
  
  # --- Optional bar plot: Word count per file with selected one highlighted ---
  output$barPlot <- renderPlot({
    df <- file_word_data()
    req(nrow(df) > 0)
    
    df$Highlight <- FALSE
    df$Highlight[input$fileIndex] <- TRUE
    
    ggplot(df, aes(x = reorder(File, Word_Count), y = Word_Count, fill = Highlight)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "steelblue")) +
      coord_flip() +
      labs(title = "Word Count per File (Highlighting Selected)",
           x = "File",
           y = "Word Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

# Run App
shinyApp(ui = ui, server = server)












# app.R

library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# -------------------------
# 1. UI
# -------------------------
ui <- fluidPage(
  titlePanel("Speaker Word Count by Conversation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("speaker", "Select Speaker:", choices = NULL)
    ),
    
    mainPanel(
      tableOutput("speakerTable"),
      plotOutput("speakerPlot")
    )
  )
)

# -------------------------
# 2. Server
# -------------------------
server <- function(input, output, session) {
  transcript_dir <- "../../data/modified_data/finalized_data"
  files <- list.files(transcript_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)
  
  # Helper to count words
  count_words <- function(text) {
    text <- na.omit(text)
    sum(str_count(text, "\\S+"))
  }
  
  # Read and process all files
  all_speaker_data <- reactive({
    speaker_data <- lapply(files, function(f) {
      df <- tryCatch({
        if (str_detect(f, "\\.csv$")) read_csv(f, show_col_types = FALSE)
        else read_tsv(f, show_col_types = FALSE)
      }, error = function(e) return(NULL))
      
      if (!is.null(df) && all(c("speech", "speaker_std") %in% names(df))) {
        df <- df %>%
          filter(!is.na(speech), !is.na(speaker_std)) %>%
          group_by(speaker_std) %>%
          summarise(
            Word_Count = sum(count_words(speech)),
            File = basename(f),
            .groups = "drop"
          )
        return(df)
      } else {
        return(NULL)
      }
    })
    
    bind_rows(speaker_data)
  })
  
  # Populate dropdown once data is loaded
  observe({
    data <- all_speaker_data()
    if (!is.null(data)) {
      speaker_choices <- sort(unique(data$speaker_std))
      updateSelectInput(session, "speaker", choices = speaker_choices)
    }
  })
  
  # Filter for selected speaker
  speaker_data_filtered <- reactive({
    req(input$speaker)
    all_speaker_data() %>% filter(speaker_std == input$speaker)
  })
  
  # Output table
  output$speakerTable <- renderTable({
    speaker_data_filtered() %>%
      select(File, Word_Count) %>%
      arrange(desc(Word_Count))
  })
  
  # Output bar chart
  output$speakerPlot <- renderPlot({
    data <- speaker_data_filtered()
    ggplot(data, aes(x = reorder(File, Word_Count), y = Word_Count)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Word Count by", input$speaker, "per Conversation"),
           x = "Conversation (File)",
           y = "Word Count") +
      theme_minimal()
  })
}

# -------------------------
# 3. Launch App
# -------------------------
shinyApp(ui = ui, server = server)







































library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Preload inventory file
inventory <- read_csv("../../data/Transcript inventory.csv", show_col_types = FALSE)
inventory <- inventory %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(date) & date >= as.Date("1990-01-01")) %>%
  mutate(num_speakers = ifelse(is.na(speakers), 0, str_count(speakers, ",") + 1))

# Identify topic columns
topic_columns <- grep("^topic_", names(inventory), value = TRUE)

# Reshape for timeline
long_data <- inventory %>%
  select(n, date, speakers, num_speakers, all_of(topic_columns)) %>%
  pivot_longer(cols = all_of(topic_columns), names_to = "topic", values_to = "present") %>%
  filter(!is.na(present)) %>%
  mutate(topic = gsub("topic_", "", topic))

# UI
ui <- fluidPage(
  titlePanel("Conversation Timeline by Topic"),
  fluidRow(
    column(12, plotOutput("timelinePlot", click = "plot_click"))
  ),
  verbatimTextOutput("popup")
)

# Server
server <- function(input, output, session) {
  output$timelinePlot <- renderPlot({
    ggplot(long_data, aes(x = date, y = topic, color = num_speakers)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_gradientn(colors = c("blue", "skyblue", "yellow", "lightgreen", "seagreen")) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(
        title = "Timeline of Conversations by Topic",
        subtitle = "Click a point to reveal speaker_std list",
        x = "Date",
        y = "Topic",
        color = "# Speakers"
      ) +
      theme_minimal()
  })
  
  observeEvent(input$plot_click, {
    req(input$plot_click)
    click_date <- as.Date(round_date(input$plot_click$x, "day"))
    click_topic <- round(input$plot_click$y)
    nearest <- long_data %>%
      mutate(dist = abs(as.numeric(date - click_date))) %>%
      filter(topic == unique(topic)[click_topic]) %>%
      arrange(dist) %>%
      slice(1)
    
    if (!is.na(nearest$n)) {
      file_path_csv <- file.path("../../data/modified_data/finalized_data", paste0(nearest$n, ".csv"))
      file_path_tsv <- file.path("../../data/modified_data/finalized_data", paste0(nearest$n, ".tsv"))
      transcript <- tryCatch({
        if (file.exists(file_path_csv)) read_csv(file_path_csv, show_col_types = FALSE)
        else if (file.exists(file_path_tsv)) read_tsv(file_path_tsv, show_col_types = FALSE)
        else NULL
      }, error = function(e) NULL)
      
      if (!is.null(transcript) && "speaker_std" %in% names(transcript)) {
        speakers <- unique(na.omit(transcript$speaker_std))
        output$popup <- renderPrint({
          cat("speaker_std's in conversation", nearest$n, ":\n")
          print(speakers)
        })
      } else {
        output$popup <- renderPrint({ cat("No speaker_std data for file", nearest$n) })
      }
    }
  })
}

# Launch App
shinyApp(ui = ui, server = server)



library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

# Helper function
count_words <- function(text) {
  text <- na.omit(text)
  sum(str_count(text, "\\S+"))
}

# --- UI ---
ui <- fluidPage(
  titlePanel("Transcript Explorer"),
  tabsetPanel(
    tabPanel("Word Count per File",
             sidebarLayout(
               sidebarPanel(uiOutput("fileSlider")),
               mainPanel(
                 textOutput("selectedFile"),
                 textOutput("wordCount"),
                 plotOutput("barPlot")
               )
             )
    ),
    tabPanel("Speaker Word Count by File",
             sidebarLayout(
               sidebarPanel(selectInput("speaker", "Select speaker_std:", choices = NULL)),
               mainPanel(
                 tableOutput("speakerTable"),
                 plotOutput("speakerPlot")
               )
             )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {
  transcript_dir <- "../../data/modified_data/finalized_data"
  all_files <- list.files(transcript_dir, pattern = "\\.(csv|tsv)$", full.names = TRUE)
  filenames <- basename(all_files)
  
  # --- Word count per file ---
  file_word_data <- reactive({
    lapply(all_files, function(file) {
      df <- tryCatch({
        if (grepl("\\.csv$", file)) read_csv(file, show_col_types = FALSE)
        else read_tsv(file, show_col_types = FALSE)
      }, error = function(e) return(NULL))
      
      wc <- if (!is.null(df) && "speech" %in% names(df)) count_words(df$speech) else NA
      data.frame(File = basename(file), Word_Count = wc)
    }) |> bind_rows()
  })
  
  output$fileSlider <- renderUI({
    sliderInput(
      inputId = "fileIndex",
      label = "Select Transcript File (by index)",
      min = 1,
      max = length(filenames),
      value = 1,
      step = 1,
      ticks = FALSE
    )
  })
  
  output$selectedFile <- renderText({
    req(input$fileIndex)
    paste("Selected File:", filenames[input$fileIndex])
  })
  
  output$wordCount <- renderText({
    req(input$fileIndex)
    wc <- file_word_data()$Word_Count[input$fileIndex]
    paste("Word Count:", wc)
  })
  
  output$barPlot <- renderPlot({
    df <- file_word_data()
    req(nrow(df) > 0)
    
    df$Highlight <- FALSE
    df$Highlight[input$fileIndex] <- TRUE
    
    ggplot(df, aes(x = reorder(File, Word_Count), y = Word_Count, fill = Highlight)) +
      geom_col() +
      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "steelblue")) +
      coord_flip() +
      labs(title = "Word Count per File (Highlighting Selected)",
           x = "File",
           y = "Word Count") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # --- Speaker word count by file ---
  all_speaker_data <- reactive({
    lapply(all_files, function(f) {
      df <- tryCatch({
        if (str_detect(f, "\\.csv$")) read_csv(f, show_col_types = FALSE)
        else read_tsv(f, show_col_types = FALSE)
      }, error = function(e) return(NULL))
      
      if (!is.null(df) && all(c("speech", "speaker_std") %in% names(df))) {
        df %>%
          filter(!is.na(speech), !is.na(speaker_std)) %>%
          group_by(speaker_std) %>%
          summarise(
            Word_Count = sum(count_words(speech)),
            File = basename(f),
            .groups = "drop"
          )
      } else NULL
    }) |> bind_rows()
  })
  
  observe({
    speakers <- sort(unique(all_speaker_data()$speaker_std))
    updateSelectInput(session, "speaker", choices = speakers)
  })
  
  speaker_data_filtered <- reactive({
    req(input$speaker)
    all_speaker_data() %>% filter(speaker_std == input$speaker)
  })
  
  output$speakerTable <- renderTable({
    speaker_data_filtered() %>%
      select(File, Word_Count) %>%
      arrange(desc(Word_Count))
  })
  
  output$speakerPlot <- renderPlot({
    df <- speaker_data_filtered()
    ggplot(df, aes(x = reorder(File, Word_Count), y = Word_Count)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = paste("Word Count by", input$speaker, "per Conversation"),
           x = "Conversation (File)",
           y = "Word Count") +
      theme_minimal()
  })
}

# --- Run App ---
shinyApp(ui = ui, server = server)










