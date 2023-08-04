import pandas as pd
import os

# Access inventory list
inventory_data = pd.read_csv('C:/Users/Alisa Fiorella/Downloads/montesinos test/inventory.tsv', delimiter='\t')

# List with topic column names .
topic_columns = ['topic_referendum', 'topic_ecuador', 'topic_lucchetti_factory', 'topic_municipal98', 'topic_reelection',
                 'topic_miraflores', 'topic_canal4', 'topic_media', 'topic_promotions', 'topic_ivcher',
                 'topic_foreign', 'topic_wiese', 'topic_public_officials', 'topic_safety', 'topic_state_capture']

average_word_count_by_topic = {}

# Iterate through each topic column and calculate the average conversation word count
for topic_column in topic_columns:
    # Filter rows with 'x' in the current topic column to get the relevant 'n' transcript numbers.
    filtered_data = inventory_data[inventory_data[topic_column] == 'x']

    # Extract the 'n' values from the filtered_data
    n_values = filtered_data['n'].tolist()

    directory_path = "C:/Users/Alisa Fiorella/Downloads/montesinos test/directory"

    total_word_count = 0
    total_conversations = 0

    # Iterate over the 'n' extracted from the filtered data
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
                word_counts = df['speech'].apply(lambda speech: len(str(speech).split()))
                # Calculate the total word count for the conversation
                conversation_word_count = word_counts.sum()

                total_word_count += conversation_word_count
                total_conversations += 1

    # Calculate the average word count for the current topic
    if total_conversations > 0:
        average_word_count = total_word_count / total_conversations
        average_word_count_by_topic[topic_column] = average_word_count

result_df = pd.DataFrame({'Topic': list(average_word_count_by_topic.keys()),
                          'Average Conversation Word Count': list(average_word_count_by_topic.values())})

# New .tsv file is created with these results.
result_df.to_csv('average_conversation_topic_word_count.csv', sep=',', index=False)