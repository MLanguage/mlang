def parse_test(f):
    with open(f, 'r') as fi:
        contents = fi.readlines()
    name = contents[1].strip()
    i = 1
    entrees = {}
    resultats = {}
    while(contents[i] != "#ENTREES-PRIMITIF\n"): i += 1
    i += 1
    while(contents[i] != "#CONTROLES-PRIMITIF\n"):
        s = contents[i].split('/')
        entrees[s[0]] = float(s[1].strip())
        i += 1
    i += 2
    while(contents[i] != "#ENTREES-CORRECTIF\n"):
        s = contents[i].split('/')
        resultats[s[0]] = float(s[1].strip())
        i += 1
    return entrees, resultats
