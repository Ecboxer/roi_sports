import requests
from bs4 import BeautifulSoup
import csv

r = requests.get ('https://datascience.berkeley.edu/analysis-cost-of-being-sports-fan/cost-of-a-game/accessible/')
c = r.content

soup = BeautifulSoup(c, features='html.parser')
leagues = ['MLB', 'NFL', 'NBA', 'NHL']
tables = soup.find_all('table')

for i in range(len(tables)):
    file_name = leagues[i] + 'cost.csv'
    with open(file_name, mode='w') as f:
        writer = csv.writer(f, delimiter=',', quotechar='"')
        
        table = tables[i]
        heading = table.thead
        features = heading.find_all('th')
        header_row = [features[i].text for i in range(len(features))]
        writer.writerow(header_row)

        body = table.tbody
        rows = body.find_all('tr')
        for row in rows:
            values = row.find_all('td')
            new_row = [values[i].text for i in range(len(values))]
            writer.writerow(new_row)
