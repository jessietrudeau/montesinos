import pandas as pd
import os

inventory_data = pd.read_csv('C:/Users/Alisa Fiorella/Downloads/montesinos test/inventory.tsv', delimiter='\t')

# List with topics
topic_columns = ['topic_referendum', 'topic_ecuador', 'topic_lucchetti_factory', 'topic_municipal98', 'topic_reelection',
                 'topic_miraflores', 'topic_canal4', 'topic_media', 'topic_promotions', 'topic_ivcher',
                 'topic_foreign', 'topic_wiese', 'topic_public_officials', 'topic_safety', 'topic_state_capture']

# This can be changed to analyze the speaking times of other people.
character_name = "MONTESINOS"

average_word_count_by_topic = {}
montesinos_word_count_by_topic = {}

# Iterate through the columns to find Montesinos' average word count per topic.
for topic_column in topic_columns:
    # Filter rows with 'x' in the current topic column to get the relevant 'n' transcript numbers.
    filtered_data = inventory_data[inventory_data[topic_column] == 'x']

    # Find 'n' values from the filtered_data
    n_values = filtered_data['n'].tolist()

    directory_path = "C:/Users/Alisa Fiorella/Downloads/montesinos test/directory"

    # Set the varibale names
    total_word_count = 0
    total_conversations = 0
    montesinos_word_count = 0
    montesinos_conversations = 0

    # Iterate over the 'n' from the filtered data
    for n in n_values:
        # Check for both .csv and .tsv files
        for file_extension in ['.csv', '.tsv']:
            filename = f"{n}{file_extension}"
            filepath = os.path.join(directory_path, filename)
            if os.path.exists(filepath) and os.path.isfile(filepath):
                separator = ',' if file_extension == ".csv" else '\t'
                # Read the file
                df = pd.read_csv(filepath, sep=separator)

                # Calculate the word count for each speech
                word_counts = df[df['speaker_std'] != 'BACKGROUND']['speech'].apply(lambda speech: len(str(speech).split()))
                # Calculate the total word count for the transcript
                conversation_word_count = word_counts.sum()

                total_word_count += conversation_word_count
                total_conversations += 1

                # Filter rows that have MONTESINOS as speaker_std
                montesinos_df = df[df['speaker_std'] == character_name]

                # Calculate the word count for Montesinos
                montesinos_word_counts = montesinos_df['speech'].apply(lambda speech: len(str(speech).split()))
                # Calculate the total word count for Montesinos
                montesinos_conversation_word_count = montesinos_word_counts.sum()

                montesinos_word_count += montesinos_conversation_word_count
                montesinos_conversations += 1

    # Average word count for the current topic
    if total_conversations > 0:
        average_word_count = total_word_count / total_conversations
        average_word_count_by_topic[topic_column] = average_word_count

    # Average word count for Montesinos
    if montesinos_conversations > 0:
        montesinos_average_word_count = montesinos_word_count / montesinos_conversations
        montesinos_word_count_by_topic[topic_column] = montesinos_average_word_count

result_df = pd.DataFrame({'Topic': list(average_word_count_by_topic.keys()),
                          'Average Conversation Word Count': list(average_word_count_by_topic.values()),
                          'Montesinos Average Word Count': list(montesinos_word_count_by_topic.values())})

# New .csv file is created with results.
result_df.to_csv('topic_vmt_avg_count.csv', sep=',', index=False)