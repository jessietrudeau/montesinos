import pandas as pd

#read the inventory file tsv
data = pd.read_csv('C:/Users/Alisa Fiorella/Downloads/montesinos test/directory/inventory.tsv', delimiter='\t')

# Replace 'topic_referendum' with the specific topic to filter by.
topic_column = 'topic_lucchetti_factory'

# Filter rows with 'x' in the specified topic column.
filtered_data = data[data[topic_column] == 'x']

#New table is created only with the transcripts that have the topic marked.
filtered_data.to_csv('test_filtered_lucchetti_data.csv', index=False)