import pandas as pd
from fuzzywuzzy import fuzz
from fuzzywuzzy import process

#Test code for 1200 transcript
# Read the TSV file into a DataFrame
df = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/1200.tsv', sep='\t')

#sample list of strings for matching
people = ["MONTESINOS", "CROUSILLAT"]

# Function to get the best match from the given list
def get_best_match(query, choices): 
    best_match = process.extractOne(query, choices) #using fuzzywuzzy for fuzzy matching of names
    return best_match[0]

# Apply fuzzy matching to match_list for each row in source_column
df['speaker_std'] = df['speaker'].apply(lambda x: get_best_match(x, people))

# Write the updated DataFrame back to a TSV file
df.to_csv('1200_2.tsv', sep='\t', index=False)

print("1200_2'")


