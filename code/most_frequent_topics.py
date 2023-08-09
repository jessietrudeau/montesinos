import pandas as pd
import os

inventory_data = pd.read_csv('C:/Users/Alisa Fiorella/Downloads/montesinos test/inventory.tsv', delimiter='\t')

# List with topics .
topic_columns = ['topic_referendum', 'topic_ecuador', 'topic_lucchetti_factory', 'topic_municipal98', 'topic_reelection',
                 'topic_miraflores', 'topic_canal4', 'topic_media', 'topic_promotions', 'topic_ivcher',
                 'topic_foreign', 'topic_wiese', 'topic_public_officials', 'topic_safety', 'topic_state_capture']

topic_total_word_count = {}
total_word_count_all_transcripts = 0

# Iterate through each topic and calculate the total word count for each topic and all transcripts
for topic_column in topic_columns:
    directory_path = "C:/Users/Alisa Fiorella/Downloads/montesinos test/directory"

    total_word_count_by_topic = 0

    # Iterate over the 'n' values from the inventory
    for n in inventory_data[inventory_data[topic_column] == 'x']['n']:
        # Check for both .csv and .tsv file extensions
        for file_extension in ['.csv', '.tsv']:
            filename = f"{n}{file_extension}"
            filepath = os.path.join(directory_path, filename)
            if os.path.exists(filepath) and os.path.isfile(filepath):
                separator = ',' if file_extension == ".csv" else '\t'
                # Read the file
                df = pd.read_csv(filepath, sep=separator)

                # Calculate the word count for each trasncript
                word_counts = df[df['speaker_std'] != 'BACKGROUND']['speech'].apply(lambda speech: len(str(speech).split()))
                # Calculate the total word count for the topic
                topic_word_count = word_counts.sum()

                total_word_count_by_topic += topic_word_count

    # Update the total word count for the current topic
    topic_total_word_count[topic_column] = total_word_count_by_topic

    # Update the total word count of all transcripts
    total_word_count_all_transcripts += total_word_count_by_topic

# Calculate the relative word count for each topic
relative_word_count_by_topic = {}
for topic_column in topic_columns:
    if total_word_count_all_transcripts > 0:
        relative_word_count = topic_total_word_count[topic_column] / total_word_count_all_transcripts
        relative_word_count_by_topic[topic_column] = relative_word_count

# Sort the relative word count for each topic in descending order
sorted_relative_word_count_by_topic = sorted(relative_word_count_by_topic.items(), key=lambda item: item[1], reverse=True)

# Convert the sorted list of tuples to a DataFrame
result_df = pd.DataFrame(sorted_relative_word_count_by_topic, columns=['Topic', 'Relative Word Count'])

# Save the result to a CSV file
result_df.to_csv('relative2_word_count_by_topic.csv', index=False)
