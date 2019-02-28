import requests
from bs4 import BeautifulSoup
import csv

r = requests.get ('https://en.wikipedia.org/wiki/Forbes%27_list_of_the_most_valuable_sports_teams')
c = r.content

soup = BeautifulSoup(c, features='html.parser')
years = ['2018', '2017', '2016', '2015', '2014', '2013', '2012', '2011', '2010']

tables = soup.find_all('table')
tables = tables[:-2]  # Remove boilerplate tables

for i in range(len(years)):
    file_name = years[i] + 'franchise_values.csv'
    with open(file_name, mode='w') as f:
        writer = csv.writer(f, delimiter=',', quotechar='"')
        
        table = tables[i]
        head = table.find('tr')
        features = head.find_all('th')
        header_row = [features[i].text.strip() for i in range(len(features))]
        writer.writerow(header_row)

        body = table.tbody
        rows = body.find_all('tr')
        rows = rows[1:]  # Exclude header row
        for j in range(len(rows)):
            if i == 0 or i == 1:
                n_features = 6
            else:
                n_features = 5
            
            values = rows[j].find_all('td')
            if len(values) == n_features:
                new_row = [values[j].text.strip() for j in range(len(values))]
            elif i == 1 and values[0].text.strip() == 'Tampa Bay Buccaneers':
                old_values = rows[j-2].find_all('td')
                old_row = [old_values[j].text.strip() for j in range(len(old_values))]
                new_row = [values[j].text.strip() for j in range(len(values))]
                new_row.insert(0, old_row[0])
            elif i == 1:
                old_values = rows[j-1].find_all('td')
                old_row = [old_values[j].text.strip() for j in range(len(old_values))]
                new_row = [values[j].text.strip() for j in range(len(values))]
                new_row.insert(0, old_row[0])
            else:
                old_values = rows[j-1].find_all('td')
                old_row = [old_values[j].text.strip() for j in range(len(old_values))]
                new_row = [values[j].text.strip() for j in range(len(values))]
                new_row.insert(0, old_row[0])
                new_row.insert(4, old_row[4])
            writer.writerow(new_row)
