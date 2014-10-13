import urllib2
import zipfile

response = urllib2.urlopen('http://files.grouplens.org/datasets/movielens/ml-100k.zip')

wf = open('temp', 'wb')
wf.write(response.read())
wf.close()

f = gzip.open('temp', 'rb')

zf = zipfile.ZipFile('temp', 'r')

pf = open('u.data','r')

