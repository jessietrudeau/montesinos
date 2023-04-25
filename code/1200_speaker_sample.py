import pandas as pd
from fuzzywuzzy import fuzz
from fuzzywuzzy import process

''' Test run to apply loop for name matching in new speaker_std column'''

# Read the TSV file into a DataFrame
df = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/1200.tsv', sep='\t')

#sample list of strings for matching (incomplete list that is just a sample)
people = ["MONTESINOS", "CROUSILLAT"]

# Function to get the best match from the given list
def get_best_match(query, choices):  #defines function that matches similar names in speaker column to names in list of people
    best_match = process.extractOne(query, choices) #using fuzzywuzzy for fuzzy matching of names
    return best_match[0]

# Apply fuzzy matching to match_list for each row in source_column
df['speaker_std'] = df['speaker'].apply(lambda x: get_best_match(x, people)) #creates new column 'speaker_std'

# Write the updated DataFrame back to a TSV file
df.to_csv('1200_2.tsv', sep='\t', index=False)



