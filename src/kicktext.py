import pandas as pd
import spacy
import re

games = (
    pd.read_csv("data/benrugg.csv", escapechar='\\',
                low_memory=False)
    .query('sub_category=="Tabletop Games"')
    .query('state in ["successful", "failed"]'))

nlp = spacy.load('en')

WSPACE_RE = re.compile('^\s*$')

def line_to_tokens(line):
    if re.match(WSPACE_RE, line):
        return
    tokens_all = []
    spaced = nlp(line)
    for sent in spaced.sents:
        tokens = []
        digit = None
        range = False
        for token in sent:
            if digit:
                if range:
                    if token.like_num:
                        tokens.append(digit + '_' + token.orth_)
                    else:
                        tokens.append(digit)
                        tokens.append('-')
                    digit = None
                    range = False
                elif token.orth_ == '-':
                    range = True
                else:
                    tokens.append(digit)
                    digit = None
            elif not token.is_punct and not token.is_space:
                if token.like_num:
                    digit = token.orth_
                else:
                    tokens.append(token.lemma_)
        tokens_all.append(' '.join(tokens))
    return tokens_all


for kid in games.kickstarter_id:
    with open('data/raw_text/' + str(kid) + '.txt', 'r') as fin:
        with open('data/lemmas/' + str(kid) + '.txt', 'w') as fout:
            for line in fin:
                parsed = line_to_tokens(line)
                if parsed:
                    for sentence in parsed:
                        fout.write(sentence + '\n')
