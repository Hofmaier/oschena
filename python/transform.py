f = open('ml-100k/u.data', 'r')
firstline = f.readline()
firstlinel = firstline.split()
currentuser = firstlinel[0]

l = list(f)

splitline = []
for line in l:
    splitline.append(line.split())

userratings = []
for line in splitline:
    if(line[0] == currentuser):
        userratings.append(line[1])

useritemratingdict = {}
for r in splitline:
    if r[0] in useritemratingdict:
        useritemratingdict[r[0]].append((r[1], r[2]))
    else:
        useritemratingdict[r[0]] = [(r[1], r[2])]

itemuserrating = {}
for r in splitline:
    if r[1] in itemuserrating:
        itemuserrating[r[1]].append((r[0], r[2]))
    else:
        itemuserrating[r[1]] = [(r[0], r[2])]        

mostrated = []
for k in itemuserrating:
    mostrated.append((len(itemuserrating[k]), k))

l = sorted(mostrated)
l.reverse()
popularitems = []
for il in l:
    x, y = il
    popularitems.append(y)
#mostrated = mostrated.sort()

useritemdict = {}
for r in splitline:
    if r[0] in useritemdict:
        useritemdict[r[0]].append(r[1])
    else:
        useritemdict[r[0]] = [r[1]]

cusers = 0
userlist = []
for k in useritemdict:
    if set(popularitems[:10]).issubset(set(useritemdict[k])):
        userlist.append(k)

itemlist = popularitems[:10]
useritemlist = []
for u in userlist:
    ratings = []
    for ir in useritemratingdict[u]:
        i, r = ir
        if i in itemlist:
            ratings.append(r)
    useritemlist.append(ratings)

lenlist = []
for r in useritemlist:
    lenlist.append(len(r))

commonratings = {}
for line in splitline:
    if line[1] in userratings:
        if line[0] in commonratings:
            commonratings[line[0]] = commonratings[line[0]] + 1
        else:
            commonratings[line[0]] = 1

m = max(commonratings.values())

identicalratings = []
for k in commonratings.keys():
    if(commonratings[k] == 19):
        identicalratings.append(k)

fw = open('fullmatrix.data', 'w')
for r in useritemlist:
        s = ' '.join(r) + '\n'
        fw.write(s)

fw.close()    
f.close()
