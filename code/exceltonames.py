import pandas as pd

file_path = "/Users/franciscasaldivar/Desktop/Strategiesz.xlsx"  # Replace with the actual file path
df = pd.read_excel(file_path)

list_of_speakers = df['speaker_std'].tolist()

print(list_of_speakers)