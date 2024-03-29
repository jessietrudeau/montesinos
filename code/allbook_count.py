''' Applies method tested in test_countwords to all transcripts, creating single table that counts words spoken by every individual'''

import pandas as pd
import os

directory_path = "C:/Users/Alisa Fiorella/Downloads/montesinos test/directory"

word_count_by_speaker = {}

# Iterate over the filenames in the directory
for filename in os.listdir(directory_path):
    # Skip any files that don't end with ".tsv"
    if not filename.endswith(".csv"):
        continue
    
    # Read the file into a DataFrame
    df = pd.read_csv(os.path.join(directory_path, filename), sep=",")
    
    # Iterate through each row in the transcript
    for index, row in df.iterrows():
        speaker = row['speaker_std']
        speech = row['speech']
        if speaker != 'BACKGROUND':
            word_count = len(str(speech).split())
            if speaker in word_count_by_speaker:
                word_count_by_speaker[speaker].append(word_count)
            else:
                word_count_by_speaker[speaker] = [word_count]

total_word_count_by_speaker = {}
total_word_count = 0

# Iterate through each speaker in the dictionary
for speaker, word_counts in word_count_by_speaker.items():
    total_word_count_by_speaker[speaker] = sum(word_counts)
    total_word_count += total_word_count_by_speaker[speaker]

# Create a a new dataframe/table with the results 
word_count_results = pd.DataFrame({
    'Speaker': list(total_word_count_by_speaker.keys()), #indicates speaker 
    'Total Word Count': list(total_word_count_by_speaker.values()), #total word count
    'Proportion of Word Count': ['{:.2%}'.format(count/total_word_count) for count in total_word_count_by_speaker.values()]
    #proportion of total word count as a percent with two decimal spots
})

# Sort the results in descending order by total word count
word_count_results = word_count_results.sort_values('Total Word Count', ascending=False)

# Save the results to a a new file
word_count_results.to_csv('book_count_results_all.tsv', sep='\t', index=False)