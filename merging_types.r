# Import file
setwd<-"C:/Users/agsotopl/Downloads/montesinos"
file<-"C:/Users/agsotopl/Downloads/Copy of Master - transcript notes - Actors.csv"
df<-read_csv(file)

#Verify column names
colnames(df)

# Ensure column names are clean
colnames(df) <- gsub(" ", "_", colnames(df))   # Replace spaces with underscores
colnames(df) <- gsub("'", "", colnames(df))    # Remove apostrophes

# Fix NA values: Ensure we replace only when necessary
df$Type_Merged <- ifelse(!is.na(df$Montesinos_inner_circle) & df$Montesinos_inner_circle == "X", 
                         "Inner Circle", 
                         ifelse(!is.na(df$Type), df$Type, NA))

# Verify the result
head(df[, c("Type", "Montesinos_inner_circle", "Type_Merged")])

# Define output file path
output_file <- "C:/Users/agsotopl/Downloads/montesinos_cleaned.csv"

# Write the cleaned dataset to a new CSV file
write_csv(df, output_file)

# Confirm the file was saved
message("Cleaned dataset saved as: ", output_file)
