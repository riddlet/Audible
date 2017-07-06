import re
import spacy
from nltk import word_tokenize

def condense_tokens(spacy_doc, audible_words, position, offset, spacy_mod):
    spacy_tokens = spacy_mod(audible_words[position+offset[1]].text)
    if 'ner' in audible_words[position+offset[1]].attrs:
        spacy_text = ''.join(j.text for j in spacy_doc[position+offset[0]:position+offset[0]+len(spacy_tokens)]) 
        spacy_tag = ''.join(j.ent_iob_ for j in spacy_doc[position+offset[0]:position+offset[0]+len(spacy_tokens)]) + '-' + \
        ''.join(j.ent_type_ for j in spacy_doc[position+offset[0]:position+offset[0]+len(spacy_tokens)]) 
        audible_text = audible_words[position+offset[1]].text 
        audible_tag = audible_words[position+offset[1]].attrs['ner']
    else:
        spacy_text = ''.join(j.text for j in spacy_doc[position+offset[0]:position+offset[0]+len(spacy_tokens)])
        spacy_tag = ''.join(j.ent_iob_ for j in spacy_doc[position+offset[0]:position+offset[0]+len(spacy_tokens)]) + '-' + \
        ''.join(j.ent_type_ for j in spacy_doc[position+offset[0]:position+offset[0]+len(spacy_tokens)])
        audible_text = audible_words[position+offset[1]].text 
        audible_tag = 'O-'
    
    offset[0] += len(spacy_tokens)-1
    return(spacy_text, spacy_tag, audible_text, audible_tag, offset)

def clean_spacy_tags(tags):
    prog = re.compile('^(B|I|O)+-(CARDINAL|PERCENT|DATE|NORP|TIME|QUANTITY|MONEY|ORDINAL|LAW|FAC|WORK_OF_ART|PRODUCT|EVENT|LANGUAGE)+$')
    if prog.match(tags):
        out = 'O-'
    else:
        out = tags
    prog = re.compile('^O+-$')
    if prog.match(out):
        out = 'O-'
    prog = re.compile('^(B|I|O)(B|I|O)*?-GPE$')
    if prog.match(out):
        matched = prog.match(out).group(1)
        out = matched+'-L'
    prog = re.compile('^((B|I)|O)+-(PERSON|ORG|LOC)+$')
    if prog.match(out):
        matched1 = prog.match(out).group(2)
        matched2 = prog.match(out).group(3)[0]
        out = matched1+'-'+matched2[0]
    return(out)

def clean_audible_tags(item):
    prog = re.compile('^O$')
    if prog.match(item):
        out = 'O-'
    else:
        out = item
    prog = re.compile('^(P|L|O)([0-9])+$')
    if prog.match(out):
        matched = '-'+prog.match(out).group(1)
        out = matched
    return(out)

def condense_stanford_tokens(audible_sentence, stanford_text, stanford_tag, audible_text, audible_tag, stanford_mod):
    offset = [0,0] #stanford, audible
    words = audible_sentence.find_all(['w', 'c'])
    doc = ' '.join(w.text.encode('utf8') for w in words)
    try:
        stanford_tags = stanford_mod.tag([doc])
    except:
        stanford_tags = stanford_mod.tag([doc.decode('ascii', 'ignore')])
    if len(stanford_tags) > len(words):
        for i, w in enumerate(words):
            stanford_words = word_tokenize(w.text)
            if len(stanford_words) == 1:
                if 'ner' in w.attrs:
                    stanford_text.append(stanford_tags[i+offset[0]][0]) 
                    stanford_tag.append(stanford_tags[i+offset[0]][1])
                    audible_text.append(w.text)
                    audible_tag.append(w.attrs['ner'])
                else:
                    stanford_text.append(stanford_tags[i+offset[0]][0]) 
                    stanford_tag.append(stanford_tags[i+offset[0]][1])
                    audible_text.append(w.text)
                    audible_tag.append('O-')
            else:
                if 'ner' in w.attrs:
                    stanford_text.append(''.join(j[0] for j in stanford_tags[i+offset[0]:i+offset[0]+len(stanford_words)])) 
                    stanford_tag.append(''.join(j[1] for j in stanford_tags[i+offset[0]:i+offset[0]+len(stanford_words)]))
                    audible_text.append(w.text)
                    audible_tag.append(w.attrs['ner'])
                else:
                    stanford_text.append(''.join(j[0] for j in stanford_tags[i+offset[0]:i+offset[0]+len(stanford_words)])) 
                    stanford_tag.append(''.join(j[1] for j in stanford_tags[i+offset[0]:i+offset[0]+len(stanford_words)]))
                    audible_text.append(w.text)
                    audible_tag.append('O-')

                offset[0] += len(stanford_words)-1
    else:
        offset[1] = len(stanford_tags)
        while len(words)>len(stanford_tags):
            try:
                doc = ' '.join(w.text.encode('utf8') for w in words[offset[1]:])
                stanford_tags.extend(stanford_mod.tag([doc]))
            except:
                doc = ' '.join(w.text.encode('utf8') for w in words[offset[1]:])
                stanford_tags.extend(stanford_mod.tag([doc.decode('ascii', 'ignore')]))
            offset[1] = len(stanford_tags)
        for i, w in enumerate(stanford_tags):   
            if 'ner' in words[i].attrs:
                stanford_text.append(w[0])
                stanford_tag.append(w[1])
                audible_text.append(words[i].text)
                audible_tag.append(words[i].attrs['ner'])
            else:
                stanford_text.append(w[0])
                stanford_tag.append(w[1])
                audible_text.append(words[i].text)
                audible_tag.append('O-')
            
    return(stanford_text, stanford_tag, audible_text, audible_tag)

def clean_stanford_tags(tags):
    prog = re.compile('^O+$')
    if prog.match(tags):
        out = 'O-'
    else:
        out = tags
    prog = re.compile('^(PERSON|LOCATION|ORGANIZATION)$')
    if prog.match(out):
        matched = '-'+prog.match(out).group(1)[0]
        out = matched
    return(out)