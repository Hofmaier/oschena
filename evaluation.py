def loaddata(filename):
    stringlines = list(open(filename, 'r'))
    splitlines = [e.split() for e in stringlines]
    return [[int(x) for x in e[:3]] for e in splitlines]

def predict(useritemlist):
    return [[x[0], x[1], averagerating] for x in useritemlist]

def itemavg(itemid, itemsdict):
    if itemid in itemsdict:
        rs = itemsdict[itemid]
        return float(sum(rs))/len(rs)
    else:
        return averagerating

def itemavgpredict(useritemlist, itemsdict):
    return [[x[0], x[1], itemavg(x[1], itemsdict)] for x in useritemlist]


def mae(predictions, ratings):
    errors = []
    for idx, e in enumerate(predictions):
        actualrating = ratings[idx]
        errors.append(actualrating[2] - e[2])

    abserrors = [abs(x) for x in errors]
    return float(sum(abserrors)) / len(abserrors)


useridcolumn = 0
itemcolumn = 1
ratingcolumn = 2
trainingdata = loaddata('ml-100k/u1.base')
testdata = loaddata('ml-100k/u1.test')
ratings = [e[ratingcolumn] for e in trainingdata]
averagerating = float(sum(ratings))/len(ratings)

inputdata = [e[:2] for e in testdata]
predictions = predict(inputdata)

avgmae = mae(predictions, testdata)

itemuserrating = {}
for r in trainingdata:
    if r[1] in itemuserrating:
        itemuserrating[r[1]].append(r[2])
    else:
        itemuserrating[r[1]] = [r[2]] 



itemavgpredictions = itemavgpredict(inputdata, itemuserrating)
itemavgmae = mae(itemavgpredictions, testdata)
