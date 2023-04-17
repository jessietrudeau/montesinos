import pandas as pd
from fuzzywuzzy import fuzz
from fuzzywuzzy import process
import requests
import os

#list of speakers created from exceltonames sheet
people = ['MONTESINOS', 'BACKGROUND', 'UNIDENTIFIED', 
          'KOURI', 'LUCCHETTI', 'FERRERO', 'FUJIMORI', 
          'BERENSON', 'SENOR', 'GUZMAN', 'PASCAL', 'URRELO', 
          'BLANCO', 'TRELLES', 'QUISPE', 'PANDO', 'VELIT', 'MARCENARO', 
          'AMOIN', 'MEDELIUS', 'RUIZ', 'SALGADO', 'JOY WAY', 'LOZADA', 'ESPICHAN', 
          'MELLADO', 'SIRURA', 'ANDRADE', 'CUCULIZA', 'DAVILA', 'MONTES DE OCA', 'SERPA', 
          'CASTILLO', 'DELLEPIANE', 'BELTRAN', 'VALLE RIESTRA', 'SANTANDER', 'ARCE', 'MARCHELO', 
          'HERNANDEZ CANELO', 'HURTADO MILLER', 'BOROBIO', 'BRINGAS', 'IVACHINE', 'SAUCHEDO SANCHEZ', 
          'HERMOZA RIOS', 'IBARCENA', 'DIANDERAS', 'BELLO VAZQUEZ', 'VILLANUEVA RUESTA', 'MONROE', 'VENERO', 
          'HUAMAN', 'TAFUR', 'GAMARRA', 'VALENZUELA', 'MAHUAD', 'VERA', 'HILDEBRANT', 'IBERICO', 'CROUSILLAT', 
          'DELGADO PARKER', 'BEDOYA', 'REATEGUI', 'ROMERO SEMINARIO', 'DIANDERAS', 'BERGAMINO', 'EMBAJADORA', 
          'LOCUTOR', 'SCHUTZ', 'NORIEGA', 'CALMELL', 'BERTINI', 'CAMPOS'
          ]

# Directory containing the documents
directory_path = "/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data"

# Loop through all files in the directory 
for filename in os.listdir(directory_path):
    # Check if the file is a tsv file 
    if filename.endswith(".tsv"):
        # Appending the filename to the directory path
        file_path = os.path.join(directory_path, filename)
        
        # Read the TSV files into a DataFrame
        df = pd.read_csv(file_path, sep='\t')
        
        # Define a get_best_match to match names in speaker list to actual speakers
        def get_best_match(query, choices):
            best_match = process.extractOne(query, choices)
            return best_match[0]
        
        # Apply fuzzy matching to 'speaker' column for each row
        df['speaker_std'] = df['speaker'].apply(lambda x: get_best_match(x, people))
        
        # Save the updated DataFrame back to the original file
        df.to_csv(file_path, sep='\t', index=False)
        
'''Changes unknown characters in the 919 transcript for "Delegacion de Israel" '''

# Read 919 TSV file into a DataFrame
df_919 = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/919.tsv', sep='\t')

# Function to change string if it contains a specific word
def speaker_switch(column, word, replacement):
    
    return column.apply(lambda x: x.replace(word, replacement) if fuzz.partial_ratio(word, x) >= 80 else x) #ratio of 80% -- high similarity

# Call the function to change the strings in 'speaker' column that contain just 'Señor' to delegacion Israel
df_919['speaker'] = speaker_switch(df_919['speaker'], 'El señor\t\t.—', 'Delegacion Israel')

'''Changes unknown characters in the 1300 and 978 series transcripts for Militar Desconocido '''

# Function to change string if it contains a specific word
def speaker_switch(column, unknown_list, replacement):

    for unknown in unknown_list:
        column = column.apply(lambda x: x.replace(unknown, replacement) if fuzz.partial_ratio(unknown, x) >= 80 else x)
    return column
#ratio of 80% ensures high similarity but not identical

# Read the 1300 TSV file into a DataFrame
military_1300 = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/1300.tsv', sep='\t')

# Read the 978 series file into a DataFrame
military_978_series = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/978_977_976_975_974.tsv', sep='\t')

# Define the unknown words and "Unidentifed Military" as the replacement word
unknown_list = ['El Oficial EP', 'El  Oficial.', 'El señor\t\t.—', 'El General FAP']
replacement = 'Militar Desconocido'

# Call the function to change the strings in 'speaker' column of the first DataFrame
military_1300['speaker'] = speaker_switch(military_1300['speaker'], unknown_list, replacement)

# Call the function to change the strings in 'speaker' column of the second DataFrame
military_978_series['speaker'] = speaker_switch(military_978_series['speaker'], unknown_list, replacement)

print(military_1300)   