#! /usr/bin/env python
import string

puncts = string.punctuation.translate(None, "'")

def clean(word):
    return word.lower().translate(None, puncts)

if __name__ == '__main__':
    import sys
    f = open(sys.argv[1])
    text = ''.join(f.readlines())

    # read the dictionary
    fd = open('/usr/share/dict/words')
    d = set(w.strip() for w in fd.readlines())
    fd.close()

    for word in text.split():
        if clean(word) not in d: print word 
        
