import pandas as pd
from urllib.request import urlopen
from lxml.html import parse
from lxml.cssselect import CSSSelector

urls = (
    pd.read_csv("data/benrugg.csv", escapechar='\\',
                low_memory=False)
    .query('sub_category=="Tabletop Games"')
    .loc[:,['kickstarter_id', 'web_url', 'blurb']]
    .set_index('kickstarter_id'))

selector = CSSSelector('div.full-description')

for tup in urls.itertuples():
    with open("data/raw_text/"+str(tup.Index)+".txt", 'w') as f:
        f.write(tup.blurb + '\n')
        content = selector(parse(urlopen(tup.web_url)))
        if len(content) > 0:
            for line in content[0]:
                text = line.text_content().strip()
                if text:
                    f.write(text + '\n')

