import pandas as pd
from fuzzywuzzy import fuzz
from fuzzywuzzy import process
import requests

#Test/sample for loop that only takes in two transcripts (706 and 857_856)

# List of GitHub TSV URLs
github_file_paths = [
    '/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/807.tsv',
    '/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/857_856.tsv'
]

people = ["MONTESINOS", "MELLADO"] #list of speakers

for file_path in github_file_paths: # Read the TSV data into a DataFrame
    tsv_data = pd.read_csv(file_path, sep='\t')
    
    def get_best_match(query, choices): #defines function that matches speaker with best choice within speaker list
        best_match = process.extractOne(query, choices)
        return best_match[0]
    
    # Apply fuzzy matching to match_list for each row in source_column
    tsv_data['speaker_std']= tsv_data['speaker'].apply(lambda x: get_best_match(x, people))

    # Save the updated DataFrame back to the TSV file
    tsv_data.to_csv(file_path.split('/')[-1], sep='\t', index=False)





    
