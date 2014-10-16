import oschena

trainingfilepath = '../ml-100k/u1.base'
testfilepath = '../ml-100k/u1.test'
userid = 1
itemid = 6
trainingset = oschena.loaddata(trainingfilepath)
testset = oschena.loaddata(testfilepath)
recommendation = oschena.predict(userid, itemid, trainingset, oschena.averagerating)
