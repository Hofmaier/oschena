def loaddata(filename):
    stringlines = list(open(filename, 'r'))
    splitlines = [e.split() for e in stringlines]
    return [[int(x) for x in e[:3]] for e in splitlines]

def averagerating(ratings, user, item):
    return float(sum([r[2] for r in ratings]))/len(ratings)

def predictions(ratings, useritemlist, predict = averagerating):
    return [[x[0], x[1], predict(ratings, x[0], x[1])] for x in useritemlist]
            
def useruser(ratings, user, item):
    useravgrating = useraveragerating(ratings, user)
    l = [sim_distance(ratings, user, u) * (rating(u, item) - useraveragerating(ratings, user)) for s, u in neighborhood(user, ratings)]
    numerator = sum(l)
    denumerator = sum([s for s, u in neighorhood(user, ratings)])
    return numerator / denumerator

def itemaverage(ratings, user, item):
    itemratings = [r[2] for r in ratings if r[1] == item]
    if len(itemratings) == 0:
        return averagerating(ratings, user, item)
    else:
        return sum(itemratings)/len(itemratings)

def shareditems(ratingspers1, ratingspers2):
    l = []
    for i in ratingspers1.keys():
        if i in ratingspers2.keys():
          l.append(i)  
    return l

def pearsonsimilarity(r, p1, p2, amean, bmean, shareditems):
    l1 = [(r[p1][i] - amean) * (r[p2][i] - bmean) for i in shareditems]
    nominator = sum(l1)
    l2 = [(r[p1][i] - amean)**2 for i in shareditems]
    s1 = sum(l2)
    dominator = sqrt(s1)  * sqrt(sum([(r[p2][i] - bmean)**2 for i in shareditems]))
    pearsonsimilarity =  nominator / dominator 
    return pearsonsimilarity

def users(ratings):
    s = []
    for r in ratings:
        if not r[0] in s:
            s.append(r[0])
    return s
            
def user_itemratingdict(ratings):
    rdict = {}
    users = users(ratings)
    for u in users:
        udict = {}
        rdict[u] = udict

    for r in ratings:
        rdict[r[0]][r[1]] = r[2]

def distance(a, b):
    return sum([(a[idx] - b[idx])**2 for idx in range(len(a))])

def userratings(items, userratingsdict):
    return [userratingsdict[i] for i in items]

def sim_distance(ratings, user1, user2):
    user_itemratingdict = user_itemratingdict(ratings)
    si = shareditems(user_itemratingdict[user1], user_itemratingdict[user2])
    ul1 = userratings(si, user_itemratingdict[user1])
    ul2 = userratings(si, user_itemratingdict[user2])
    return distance(ul1, ul2)

def sim(u1, u2, R):
    sl = shareditems(R[u1], R[u2])

def neighborhood(user, ratings, size = 5):
    users = users(ratings)
    l= []
    for u in users(ratings):
        if u != user:
            l.append(( sim_distance(ratings, user, u), u))
    
    l.sort()
    return l[:n]

def useraveragerating(ratings, user):
    ul = [r[2] for r in ratings if r[0] == user] 
    return sum(ul) / len(ul)

def rating(ratings, user, item):
    for r in ratings:
        if r[0] == user and r[1] == item:
            return r[2]
        else:
            return None

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
trainingdata = loaddata('../ml-100k/u1.base')
testdata = loaddata('../ml-100k/u1.test')
ratings = [e[ratingcolumn] for e in trainingdata]


inputdata = [e[:2] for e in testdata]

#avgmae = mae(predictions, testdata)

itemuserrating = {}
for r in trainingdata:
    if r[1] in itemuserrating:
        itemuserrating[r[1]].append(r[2])
    else:
        itemuserrating[r[1]] = [r[2]] 

useritemrating = {}
for r in trainingdata:
    if r[0] in useritemrating:
        useritemrating[r[0]].append(r[2])
    else:
        useritemrating[r[0]] = [r[0]]

items = itemuserrating.keys().sort()

rdict = {}
for u in users(trainingdata):
    udict = {}
    rdict[u] = udict

for r in trainingdata:
    rdict[r[0]][r[1]] = r[2]

R = []
for i in range(len(users(trainingdata))):
    R.append(None)

for u in users(trainingdata):
    ui = []
    for i in itemuserrating.keys():
        ui.append(None)
    R.append(ui)

avgpredictions = predictions(trainingdata, inputdata)
avgmae = mae(avgpredictions, testdata)
itemavgpredictions = predictions(trainingdata, inputdata, itemaverage )
#itemavgmae = mae(itemavgpredictions, testdata)
#useruserpredictions = useruserpredict(inputdata, trainingdata)


