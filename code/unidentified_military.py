import pandas as pd
from fuzzywuzzy import fuzz

'''Changes unknown characters in the 1300 and 978 series transcripts for Militar Desconocido '''

# Function to change string if it contains a specific word
def speaker_switch(column, unknown_list, replacement): #create function that takes in column, original word and replacement word
    for unknown in unknown_list:
        column = column.apply(lambda x: x.replace(unknown, replacement) if fuzz.partial_ratio(unknown, x) >= 80 else x) #ratio of 80% ensures high similarity but not identical
    return column

# Read the 1300 TSV file into a DataFrame
military_1300 = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/1300.tsv', sep='\t')
# Read the 978 series file into a DataFrame
military_978_series = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/978_977_976_975_974.tsv', sep='\t')

# Define the unknown words and define replacement word as "Unidentifed Military"
unknown_list = ['El Oficial EP', 'El  Oficial.', 'El señor\t\t.—', 'El General FAP']
replacement = 'Militar Desconocido'

# Call the function to change the strings in 'speaker' column to 'Military Desconocido' in 1300
military_1300['speaker'] = speaker_switch(military_1300['speaker'], unknown_list, replacement)
# Call the function to change the strings in 'speaker' column to 'Military Desconocido' in 978
military_978_series['speaker'] = speaker_switch(military_978_series['speaker'], unknown_list, replacement)

# Save the updated dataframes back to the original files
military_1300.to_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/1300.tsv', sep='\t', index=False)
military_978_series.to_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/978_977_976_975_974.tsv', sep='\t', index=False)