''' Applies method tested in test_countwords to all transcripts, creating single table that counts words spoken by every individual'''

import pandas as pd
import os

directory_path = "C:/Users/Alisa Fiorella/Downloads/montesinos test/directory"

speaking_frequency = {}

# Iterate over the filenames in the directory
for filename in os.listdir(directory_path):
    # Skip any files that don't end with ".csv" or ".tsv"
    if not (filename.endswith(".csv") or filename.endswith(".tsv")):
        continue

    # Determine the separator based on the file extension
    separator = ',' if filename.endswith(".csv") else '\t'

    # Read the file into a DataFrame
    df = pd.read_csv(os.path.join(directory_path, filename), sep=separator)

    # .unique will make sure the speakers are only counted once per transcript even if they have multiple lines
    unique_speakers = df['speaker_std'].unique()
    
    # Iterate through unique speakers to find how many transcripts they appear in.
    for speaker in unique_speakers:
        if speaker != 'BACKGROUND':
            if speaker in speaking_frequency:
                speaking_frequency[speaker] += 1
            else:
                speaking_frequency[speaker] = 1

speaking_frequency_results = pd.DataFrame({
    'Speaker': list(speaking_frequency.keys()), # indicates speaker
    'Frequency': list(speaking_frequency.values()) # number of times spoken
})

# Sort the results in descending order by who appears in more transcripts
speaking_frequency_results = speaking_frequency_results.sort_values('Frequency', ascending=False)

# Save the results to a a new file
speaking_frequency_results.to_csv('speaker_frequency_results.csv', sep=',', index=False)