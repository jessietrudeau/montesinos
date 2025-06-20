#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(lubridate)

# Helper function
count_words <- function(text) {
  text <- na.omit(text)
  sum(str_count(text, "\\S+"))
}

# Preload inventory data for timeline tab
inventory <- read_csv("../../data/Descriptions.csv", show_col_types = FALSE)
inventory <- inventory %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(date) & date >= as.Date("1990-01-01")) %>%
  mutate(num_speakers = ifelse(is.na(speakers), 0, str_count(speakers, ",") + 1))
topic_columns <- grep("^topic_", names(inventory), value = TRUE)
long_data <- inventory %>%
  pivot_longer(
    cols = all_of(topic_columns),
    names_to = "topic",
    values_to = "present"
  ) %>%
  mutate(
    present = str_trim(tolower(as.character(present))),
    topic = gsub("topic_", "", topic)
  ) %>%
  filter(present %in% c("x", "1", "yes")) %>%
  distinct(n, topic, .keep_all = TRUE)  # ensures one row per (n, topic)


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
    ),
    tabPanel("Timeline by Topic",
             fluidRow(
               column(12, plotOutput("timelinePlot", click = "plot_click"))
             ),
             uiOutput("popup")
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
  
  # --- Timeline Plot and speaker_std popup ---
  output$timelinePlot <- renderPlot({
    ggplot(long_data, aes(x = date, y = topic, color = num_speakers)) +
      geom_point(size = 3, alpha = 0.8) +
      scale_color_gradientn(colors = c("blue", "skyblue", "yellow", "lightgreen", "seagreen")) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(title = "Timeline of Conversations by Topic",
           subtitle = "Click a point to reveal speaker_std list",
           x = "Date", y = "Topic", color = "# Speakers") +
      theme_minimal()
  })
  
  observeEvent(input$plot_click, {
    req(input$plot_click)
    click_date <- as.Date(input$plot_click$x)
    click_topic_index <- round(input$plot_click$y)
    
    # Find closest match based on date and topic
    nearest <- long_data %>%
      mutate(dist = abs(as.numeric(date - click_date))) %>%
      filter(topic == unique(topic)[click_topic_index]) %>%
      arrange(dist) %>%
      slice(1)
    
    if (!is.na(nearest$n)) {
      # Match description from inventory by `n`
      desc <- inventory %>%
        filter(n == nearest$n) %>%
        pull(summary)
      
      output$popup <- renderUI({
        desc <- inventory %>%
          filter(n == nearest$n) %>%
          pull(summary)
        
        if (length(desc) == 0 || is.na(desc)) {
          HTML("<p><em>No description available for this conversation.</em></p>")
        } else {
          HTML(paste0("<div style='white-space: pre-wrap; max-width: 800px;'>",
                      "<strong>Description for conversation ", nearest$n, ":</strong><br><br>",
                      desc,
                      "</div>"))
        }
      })
    }
  })
}

# --- Run App ---
shinyApp(ui = ui, server = server)

