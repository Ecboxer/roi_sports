import requests
from bs4 import BeautifulSoup
import csv

r = requests.get ('https://en.wikipedia.org/wiki/List_of_professional_sports_leagues_by_revenue')
c = r.content

soup = BeautifulSoup(c, features='html.parser')

tables = soup.find_all('table')
table = tables[0]

file_name = 'league_revenue.csv'
with open(file_name, mode='w') as f:
    writer = csv.writer(f, delimiter=',', quotechar='"')
    
    header = table.find('tr')
    header_values = header.find_all('th')
    header_row = [header_values[i].text.strip() for i in range(len(header_values))]
    writer.writerow(header_row)

    rows = table.find_all('tr')
    rows = rows[1:]
    for i in range(len(rows)):
        row = rows[i]
        values = row.find_all('td')
        new_row = [values[i].text.strip() for i in range(len(values))]
        writer.writerow(new_row)
