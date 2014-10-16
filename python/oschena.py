import sys
def loaddata(filename):
    stringlines = list(open(filename, 'r'))
    splitlines = [e.split() for e in stringlines]
    return [[int(x) for x in e[:3]] for e in splitlines]

def averagerating(user, item, ratings):
    """Returns the average of all ratings.
    >>> ratings = [[1,1,1], [1,1,9]]
    >>> averagerating(1,1,ratings)
    5.0
    """

    return float(sum([r[2] for r in ratings]))/len(ratings)

def predict(user, item, trainingset, recommender = averagerating):
    return recommender(user, item, trainingset)

def distance(a, b):
    """ Returns squared euclidean distance between u and v

    >>> u = [1,2,3]
    >>> v = [2,4,6]
    >>> distance(u,v)
    14
    """
    return sum([(a[idx] - b[idx])**2 for idx in range(len(a))])

def users(ratings):
    """Returns set of user ids
    >>> ratings = [[1, 268, 5], [1, 269, 5], [1, 270, 5], [1, 271, 2], [2, 1, 4], [2, 10, 2], [2, 14, 4], [2, 25, 4], [2, 100, 5], [2, 111, 4], [2, 127, 5], [2, 237, 4], [2, 242, 5], [2, 255, 4], [2, 258, 3], [2, 269, 4], [2, 272, 5], [2, 273, 4], [2, 274, 3], [2, 275, 5], [2, 276, 4], [2, 277, 4], [2, 278, 3], [2, 282, 4], [2, 283, 5], [2, 306, 4], [2, 309, 1], [2, 310, 4], [2, 311, 5], [3, 181, 4], [3, 258, 2], [3, 260, 4], [3, 268, 3], [3, 271, 3], [3, 288, 2], [3, 302, 2], [3, 303, 3], [3, 317, 2], [3, 319, 2], [3, 320, 5], [3, 321, 5], [3, 322, 3], [3, 325, 1], [3, 326, 2]]
    >>> users(ratings)
    [1, 2, 3]

    """
    
    s = []
    for r in ratings:
        if not r[0] in s:
            s.append(r[0])
    return s

def neighborhood(user, ratings, user_itemratingdict, size=5):
    ul = users(ratings)
    ul.remove(user)

    l = [(sim_distance(ratings, user, x, user_itemratingdict), x) for x in ul]

    l.sort()
    print l[:5]
    return l[:50]

def sim_distance(ratings, user1, user2, user_itemratingdict):
    si = shareditems(user_itemratingdict[user1], user_itemratingdict[user2])
    if len(si) < 5:
        return sys.maxint
    ul1 = userratings(si, user_itemratingdict[user1])
    ul2 = userratings(si, user_itemratingdict[user2])
    d = 1/float(sqrt(distance(ul1, ul2)))

    return d

def create_user_itemratingdict(ratings):
    rdict = {}
    ul = users(ratings)
    for u in ul:
        udict = {}
        rdict[u] = udict

    for r in ratings:
        rdict[r[0]][r[1]] = r[2]
    return rdict

def shareditems(ratingspers1, ratingspers2):
    l = []
    for i in ratingspers1.keys():
        if i in ratingspers2.keys():
          l.append(i)  
    return l

def userratings(items, userratingsdict):
    return [userratingsdict[i] for i in items]

def useraveragerating(ratings, user):
    ul = [r[2] for r in ratings if r[0] == user] 
    return sum(ul) / len(ul)
            
def useruser(ratings, user):
    user_itemratingdict = create_user_itemratingdict(ratings)
    useravgrating = useraveragerating(ratings, user)
    itemtotal = {}
    simsum = {}
    for s, u in neighborhood(user, ratings, user_itemratingdict):
        for i in user_itemratingdict[u].keys():
            itemtotal.setdefault(i,0)
            simsum.setdefault(i,0)
            itemtotal[i] += s * user_itemratingdict[u][i]
            simsum[i] += s

    rankings = []

    for i, total in itemtotal.items():
        s = simsum[i]
        p = total/s
        rankings.append((p,i))

    rankings.sort()
    rankings.reverse()
    return rankings

def useruserp(user, item, uird, ratings, neighborhood):
    avg = useraveragerating(ratings, user)
    hr = haverated(item, neighborhood, uird)
    l = []
    for s, u in hr:
        uavg = useraveragerating(ratings, u)
        p = s * (uird[u][item] - uavg)
        l.append(p)

    numerator = sum(l)
    dominator = sum([s for s, u in hr])
    return avg + float(numerator)/dominator
        
    
def haverated(item, neighborhood, uird):
    l = []
    for s, u in neighborhood:
        if item in uird[u]:
            l.append((s, u))
    return l

if __name__ == "__main__":
    import doctest
    doctest.testmod()
