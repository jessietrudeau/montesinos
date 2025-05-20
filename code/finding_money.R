



library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(purrr)

# Set the path to your folder
folder_path <- "../data/modified_data/finalized_data"

# Get list of all .csv and .tsv files
file_list <- list.files(path = folder_path, pattern = "\\.(csv|tsv)$", full.names = TRUE)

# Function to process a single file
process_file <- function(file_path) {
  delim <- ifelse(grepl("\\.csv$", file_path), ",", "\t")
  file_name <- basename(file_path)
  
  # Try reading the file
  tryCatch({
    df <- read_delim(file_path, delim = delim, col_types = cols())
    
    # Loop through character columns
    results <- map_dfr(names(df), function(col_name) {
      if (is.character(df[[col_name]]) || is.factor(df[[col_name]])) {
        df %>%
          mutate(row_number = row_number(),
                 speech = as.character(.data[[col_name]])) %>%
          filter(str_detect(speech, fixed("$")) | str_detect(str_to_lower(speech), "dolares")) %>%
          mutate(filename = file_name) %>%
          select(row_number, speech, filename)
      } else {
        tibble()
      }
    })
    return(results)
  }, error = function(e) {
    message("Error reading file: ", file_name)
    return(tibble())
  })
}

# Apply to all files and combine
final_results <- map_dfr(file_list, process_file)

# Print or export
print(final_results)
# write_csv(final_results, "money_mentions.csv")  # Uncomment to save output
