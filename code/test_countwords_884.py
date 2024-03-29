'''' Test code to count the words in the speech column and match them to the names in speaker_std and create a new file with results'''
'''Working with transcript 884 with Dellepiane and Montesinos'''

import pandas as pd

# Read the TSV file into a DataFrame
df_884 = pd.read_csv('/Users/franciscasaldivar/Desktop/Montesinos_GitHub/montesinos/data/modified_data/modified_data/884.tsv', sep='\t')

word_count_by_speaker = {} #creating a dictionary variable of the number of words by speaker

# Iterate through each row in the transcript
for index, row in df_884.iterrows(): #iterating over every row in the data frame
    speaker = row['speaker_std'] #defining speaker column
    speech = row['speech'] #defining speech column
    if speaker != 'BACKGROUND': #exclude Background from being counted as a speaker
        word_count = len(speech.split()) 
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

#results of the code above, creating three lists based on unique speakers in speaker_std, total word count and percentage of words by each speaker
word_count_results_884 = pd.DataFrame({
    'Speaker': list(total_word_count_by_speaker.keys()),
    'Total Word Count': list(total_word_count_by_speaker.values()),
    'Proportion of Word Count': [count/total_word_count for count in total_word_count_by_speaker.values()]
})

# Writing the results into a new TSV file that is saved with the other transcripts
word_count_results_884.to_csv('word_count_results_884_noBackground.tsv', sep='\t', index=False)