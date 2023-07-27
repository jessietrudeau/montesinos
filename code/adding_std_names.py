import pandas as pd
from fuzzywuzzy import fuzz
from fuzzywuzzy import process
import requests
import os

'''Iterates every transcript in the file to create a speaker_std column that matches name of speaker to people in list below'''

#list of speakers created from exceltonames sheet
people = ['MONTESINOS', 'Desconocido', 'BACKGROUND', 'ALEX KOURI', 'LUCCHETTI', 'FERRERO', 'FUJIMORI', 'BERENSON', 
          'GUZMAN', 'PASCAL', 'URRELO', 'BLANCO', 'TRELLES', 'QUISPE', 'PANDO', 'VELIT', 
          'MARCENARO', 'AMOIN', 'MEDELIUS', 'RUIZ', 'SALGADO', 'JOY WAY', 'LOZADA', 'ESPICHAN', 
          'MELLADO', 'SIRURA', 'ANDRADE', 'CUCULIZA', 'DAVILA', 'MONTES DE OCA', 'SERPA', 'CASTILLO', 
          'DELLEPIANE', 'BELTRAN', 'VALLE RIESTRA', 'SANTANDER', 'ARCE', 'MARCHELO', 'HERNANDEZ CANELO', 
          'HURTADO MILLER', 'BOROBIO', 'BRINGAS', 'IVACHINE', 'SAUCEDO SANCHEZ', 'HERMOZA RIOS', 
          'IBARCENA', 'DIANDERAS', 'BELLO VAZQUEZ', 'VILLANUEVA RUESTA', 'MONROE', 'VENERO', 'HUAMAN', 
          'TAFUR', 'GAMARRA', 'VALENZUELA', 'MAHUAD', 'VERA', 'HILDEBRANT', 'IBERICO', 'CROUSILLAT', 
          'DELGADO PARKER', 'BEDOYA', 'REATEGUI', 'ROMERO SEMINARIO', 'DIANDERAS', 'BERGAMINO', 
          'EMBAJADORA', 'LOCUTOR', 'SCHUTZ', 'NORIEGA', 'CALMELL', 'BERTINI', 'CAMPOS', 'ISRAEL', 
          'MILITAR DESCONOCIDO', 'CRNL RAMIREZ', 'PORTILLO', 'COSTA', 'ARANCIBIA', 'IBÁRCENA', 
          'BOLONA', 'TUDELA', 'MACCAFFREY', 'PANDOLFI', 'JETT', 'LEWIS', 'PERCOVICH', 'LUPIS', 'RICKETTS', 
          'SUBOFICIAL PÉREZ', 'GUILLEN', 'GONZALES', 'DUNN', 'ROZAS', 'HEATHER', 'SILVA', 'PALERMO', 'CHIRINOS', 
          'VIDAL', 'MOROTE', 'AMERICANO', 'RODRIGUEZ', 'LOPEZ', 'VILLAREAL', 'SCHüTZ', 'SENOR', 'SEÑOR',
          'mozo', 'SEÑORA', 'MALEA', 'VIVANCO', 'MARINO COSTA', 'VERA ABAD', 'ALVA', 'BURNET', 'GARCIA',
          'BURNEI', 'FRANCISCO KOURI', 'SILVA RUETE', 'GENERAL DELGADO', 'PANTOJA', 'ALBERTO KOURI', 
          'DUFOUR', 'CENEPO', 'SEÑORITA', 'DE LOPEZ', 'SMITH', 'VILLAREAL', 'AYBAR', 'MANUEL LOPEZ', 'SANCHEZ']

# Directory containing the documents
directory_path = 'C:/Users/Alisa Fiorella/Downloads/montesinos test/directory'

# Loop through all files in the directory
for filename in os.listdir(directory_path):
    # Check if the file is a csv file
    if filename.endswith(".csv"):
        file_path = os.path.join(directory_path, filename)
        try:
            # Read the CSV files into a DataFrame with different encodings and separators
            df = pd.read_csv(file_path, sep=',', encoding='utf-8-sig')

            # Rest of the code for fuzzy matching and saving the DataFrame
        except pd.errors.ParserError as e:
            print(f"Error processing file '{filename}': {e}") #line to be able to find any files that have an error
            continue  # Skip the current file and continue with the next one

        # Define a get_best_match to match names in speaker list to actual speakers
        def get_best_match(query, choices):
            best_match = process.extractOne(query, choices)
            return best_match[0]

        # Apply fuzzy matching to 'speaker' column for each row in every transcript, creating new column that outputs name code
        df['speaker_std'] = df['speaker'].apply(lambda x: get_best_match(x, people))

        # Replaces all unknown "señor" in speaker column with Desconocido
        words_to_replace = ['El señor', 'SENOR', 'SEÑOR', 'mozo', 'SEÑORA', 'intérprete', 'SEÑORITA']
        df['speaker'] = df['speaker'].apply(lambda x: x.replace(x, 'Desconocido') if x in words_to_replace else x)
        df['speaker_std'] = df['speaker_std'].apply(lambda x: x.replace(x, 'DESCONOCIDO') if x in words_to_replace else x)
        # Save the updated DataFrame back to the original file
        df.to_csv(file_path, sep=',', index=False)