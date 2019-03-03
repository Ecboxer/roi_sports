import requests
from bs4 import BeautifulSoup
import csv

r = requests.get ('https://www.wcpo.com/news/insider/taking-a-look-at-what-major-league-soccer-teams-charge-for-tickets')
c = r.content

soup = BeautifulSoup(c, features='html.parser')
article = soup.find('article')
div = article.find('div', class_='left-column')
div_1 = div.find('div', class_='ArticlePage-articleBody')
div_2 = div_1.find('div', class_='RichTextArticleBody-body')

p_list = div_2.find_all('p')
p_list = p_list[13:]  # Remove article and non-standard first entry

file_name = 'mls_prices.csv'
with open(file_name, mode='w') as f:
    writer = csv.writer(f, delimiter=',', quotechar='"')
    
    header_row = ['Team', 'Supporters group season tickets',
                  'Single-game tickets', 'Secondary market average price']
    writer.writerow(header_row)
    cinc_row = ['FC Cincinnati', '$170', '$10-60', 'N/A']
    writer.writerow(cinc_row)
    
    row = []
    for i in range(len(p_list)):
        if i % 4 == 0:
            row.append(p_list[i].strong.text)
        if i % 4 != 0:
            text = p_list[i].text
            i_colon = text.find(':')
            value = text[i_colon+1:].strip()
            row.append(value)
        if i % 4 == 3:
            writer.writerow(row)
            row = []
