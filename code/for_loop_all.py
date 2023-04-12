import pandas as pd
from fuzzywuzzy import fuzz
from fuzzywuzzy import process
import requests
import os

people = ["MONTESINOS", "CROUSILLAT", "MELLADO"]

# Directory containing the documents
directory_path = "/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data"

# Loop through all files in the directory
for filename in os.listdir(directory_path):
    # Check if the file is a text file (you can modify this condition to match your specific file format)
    if filename.endswith(".tsv"):
        # Construct the full file path by appending the filename to the directory path
        file_path = os.path.join(directory_path, filename)
        
        # Read the TSV file into a DataFrame
        df = pd.read_csv(file_path, sep='\t')
        
        # Define a function to get the best match using fuzzy matching
        def get_best_match(query, choices):
            best_match = process.extractOne(query, choices)
            return best_match[0]
        
        # Apply fuzzy matching to 'speaker' column for each row
        df['speaker_std'] = df['speaker'].apply(lambda x: get_best_match(x, people))
        
        # Save the updated DataFrame back to the original file
        df.to_csv(file_path, sep='\t', index=False)