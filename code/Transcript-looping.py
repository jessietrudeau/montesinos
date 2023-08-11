#With loop
import csv

def convert_text_to_csv(input_file, output_file, delimiter):
    with open(input_file, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    with open(output_file, 'w', newline='', encoding='utf-8-sig') as csvfile:
        writer = csv.writer(csvfile)
        for line in lines:
            line = line.strip()
            if delimiter in line:
                speaker, dialogue = line.split(delimiter, 1)
                writer.writerow([speaker.strip(), dialogue.strip()])
            else:
                writer.writerow([line])

# List of input file names
input_files = ['351 - A (1).txt', '351 - A side B.txt', '351-A.txt', '352-A.txt', '353.txt', 
               '848.txt', '894.txt', '896.txt', '924.txt', '925.txt', '926.txt', '1014-1015.txt', 
               '1016.txt', '1017.txt', '1172.txt', '1174.txt', '1177.txt', '1182.txt', '1312.txt', 
               '1324.txt', '1364.txt', '1365.txt', '1557.txt', '1583.txt', '1613 - A.txt', '1663.txt',
               '1692.txt', '1780.txt', '1782.txt', '1798.txt', '1806.txt', '1809.txt', 'A15-B8.txt',
               'B 139 A-4.txt', 'B_1-B.txt', 'B-139 -4A.txt', 'C-72 (1).txt', 'C-72 (2).txt', 'C-72.txt',
               'n_a.txt', 'P_I Casete C.txt', 'P_I Casete D.txt', 'P_I Casete E.txt', 'P_I Siberia Casete B.txt',
               'sn_sf.txt', 's_n.txt']
delimiter = '.—'

for input_file in input_files:
    output_file = input_file.replace('.txt', '.csv')
    convert_text_to_csv(input_file, output_file, delimiter)
