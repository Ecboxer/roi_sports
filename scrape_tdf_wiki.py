import requests
from bs4 import BeautifulSoup
import csv

r = requests.get ('https://en.wikipedia.org/wiki/List_of_Tour_de_France_general_classification_winners')
c = r.content

soup = BeautifulSoup(c, features='html.parser')

tables = soup.find_all('table')
table = tables[2]  # Actual data

with open('tdf_times.csv', mode='w') as f:
    writer = csv.writer(f, delimiter=',', quotechar='"')
    
    heading = table.tr.find_all('th')
    header_row = [heading[i].text.strip() for i in range(len(heading))]
    writer.writerow(header_row)

    rows = table.find_all('tr')
    rows = rows[1:]
    for i in range(len(rows)):
        row = rows[i]
        row_values = row.find_all('td')
        new_row = [row_values[j].text.strip() for j in range(len(row_values))]
        try:
            rider = row.find('th').a.text
        except AttributeError:
            rider = 'None'
        if len(new_row) == 1:
            for _ in range(7):
                new_row.append('None')
        new_row.insert(2, rider)
        writer.writerow(new_row)
