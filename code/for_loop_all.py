import pandas as pd
from fuzzywuzzy import fuzz
from fuzzywuzzy import process
import requests
import os

#list of speakers created from exceltonames sheet
people = ['MONTESINOS', 'DESCONOCIDO', 'KOURI', 'LUCCHETTI', 'FERRERO', 'FUJIMORI', 'BERENSON', 
          'SENOR', 'GUZMAN', 'PASCAL', 'URRELO', 'BLANCO', 'TRELLES', 'QUISPE', 'PANDO', 'VELIT', 
          'MARCENARO', 'AMOIN', 'MEDELIUS', 'RUIZ', 'SALGADO', 'JOY WAY', 'LOZADA', 'ESPICHAN', 
          'MELLADO', 'SIRURA', 'ANDRADE', 'CUCULIZA', 'DAVILA', 'MONTES DE OCA', 'SERPA', 'CASTILLO', 
          'DELLEPIANE', 'BELTRAN', 'VALLE RIESTRA', 'SANTANDER', 'ARCE', 'MARCHELO', 'HERNANDEZ CANELO', 
          'HURTADO MILLER', 'BOROBIO', 'BRINGAS', 'IVACHINE', 'SAUCHEDO SANCHEZ', 'HERMOZA RIOS', 'IBARCENA', 'DIANDERAS', 
          'BELLO VAZQUEZ', 'VILLANUEVA RUESTA', 'MONROE', 'VENERO', 'HUAMAN', 'TAFUR', 'GAMARRA', 'VALENZUELA', 
          'MAHUAD', 'VERA', 'HILDEBRANT', 'IBERICO', 'CROUSILLAT', 'DELGADO PARKER', 'BEDOYA', 'REATEGUI', 'ROMERO SEMINARIO', 
          'DIANDERAS', 'BERGAMINO', 'EMBAJADORA', 'LOCUTOR', 'SCHUTZ', 'NORIEGA', 'CALMELL', 'BERTINI', 
          'CAMPOS', 'ISRAEL', 'MILITAR DESCONOCIDO', 'CRNL RAMIREZ', 'PORTILLO']

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

        # Replace word in 'column_name' column if it is included in 'words_to_replace' list
        words_to_replace = ['El señor\t\t.—']
        df['speaker'] = df['speaker'].apply(lambda x: x.replace(x, 'Desconocido') if x in words_to_replace else x)

        # Save the updated DataFrame back to the original file
        df.to_csv(file_path, sep='\t', index=False)
    