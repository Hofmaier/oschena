s = ''
f = open('ml-100k/u.data', 'r')
for _ in range(5):
    l = f.readline()
    s = s + l


fw = open('fivelines.data', 'w')
fw.write(s)
f.close()
fw.close()
