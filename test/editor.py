#!/usr/bin/env python

import sys

cube = set()
showingPlane = 0

if len(sys.argv) < 2:
    print 'usage: editor.py file'
    sys.exit(1)

def getbitfrom(x,y,z,bounds,data):
    bitnumber = z + (y * bounds) + (x * bounds * bounds)
    bytenumber = bitnumber / 8
    bitnumber = bitnumber % 8
    thebyte = ord(data[bytenumber])
    return ((thebyte >> bitnumber) % 2)
    
file = open(sys.argv[1],'rb')
bounds = ord(file.read(1)[0])
data = file.read()
for x in range(bounds):
    for y in range(bounds):
        for z in range(bounds):
            if getbitfrom(x,y,z,bounds,data):
                cube.add((x,y,z))

def reverseBits(x):
    y = 0
    for i in range(8):
        y = y * 2 + (x % 2)
        x /= 2
    return y
                
def makecube(bounds,cube):
    bits = 0
    count = 0
    res = []
    for x in range(bounds):
        for y in range(bounds):
            for z in range(bounds):
                count = count + 1
                if (x,y,z) in cube:
                    bits = (bits * 2) + 1
                else:
                    bits = bits * 2
                if count == 8:
                    res.append(reverseBits(bits))
                    count = 0
                    bits = 0
    if count > 0:
        res.append(bits)
    res = [chr(x) for x in res]
    res = chr(bounds) + ''.join(res)
    return res

def showPlane(y,cube):
    res = ""
    for z in range(bounds):
        if z != 0:
            res += "|\n"
        for x in range(bounds):
            if (x,y,z) in cube:
                res += "@@"
            else:
                res += "  "
    return res

print ('bounds %s' % bounds)

l = "start"
while l != "" and l != "done":
    line = l.strip()
    words = line.split()
    try:
        if words[0] == 'save':
            print 'saving...'
            cdata = makecube(bounds,cube)
            f = open(words[1],'wb')
            f.write(cdata)
            f.close()
        elif words[0] == 'up':
            showingPlane = min(showingPlane + 1, bounds - 1)
        elif words[0] == 'down':
            showingPlane = max(showingPlane - 1, 0)
        else:
            if len(words) == 3:
                x = int(words[0])
                y = int(words[1])
                z = int(words[2])
                if (x,y,z) in cube:
                    cube.remove((x,y,z))
                else:
                    cube.add((x,y,z))
            elif len(words) == 7:
                x1 = int(words[1])
                y1 = int(words[2])
                z1 = int(words[3])
                x2 = int(words[4])
                y2 = int(words[5])
                z2 = int(words[6])
                for x in range(x1,x2+1):
                    for y in range(y1,y2+1):
                        for z in range(z1,z2+1):
                            if words[0] == 'erase':
                                if (x,y,z) in cube:
                                    cube.remove((x,y,z))
                            elif words[0] == 'fill':
                                cube.add((x,y,z))
                            
    except:
        print('exception')
    print('plane %s' % showingPlane)
    print(showPlane(showingPlane,cube))
    l = sys.stdin.readline()
