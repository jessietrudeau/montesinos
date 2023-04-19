import pandas as pd

#Takes in column in Strategies excel with speaker names and transforms to list
file_path = "/Users/franciscasaldivar/Desktop/Strategiesz.xlsx"  #File path for excel sheet (saved in personal Desktop)
df = pd.read_excel(file_path)

list_of_speakers = df['speaker_std'].tolist() #transforms column to list

print(list_of_speakers) #prints list of speakers in Terminal, copy paste into loop