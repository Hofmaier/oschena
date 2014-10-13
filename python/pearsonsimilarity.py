from math import sqrt
def mean(l):
    return float(sum(l))/len(l)

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

a = [5,3,4,4]
b = [3,1,2,3,3]
u2 = [4, 3, 4, 3]
u3ratings = [1, 5, 5, 2]

amean = float(mean(a))
bmean = float(mean(b))
u2mean = float(mean(u2))
u3mean = float(mean(u3ratings))

shareditems = [0,1,2,3]
pers1 = {}
pers2 = {}
for i in shareditems:
    idx = i -1
    pers1[i] = a[idx]
    pers2[i] = b[idx]

r = []
p1 = 0
p2 = 1
u2id = 2
u3id = 3
r.append(a)
r.append(b[:4])
r.append(u2)
r.append(u3ratings)

p = pearsonsimilarity(r, p1, p2, amean, bmean, shareditems)
pears1 = pearsonsimilarity(r, p1, u2id, amean, u2mean, shareditems)
pears2 = pearsonsimilarity(r, p1, u3id, amean, u3mean, shareditems)
