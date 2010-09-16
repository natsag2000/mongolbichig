#!/usr/bin/env python
#
# only < uni1842 letters
#
import sys
import fontforge
NAMELIST = "/tmp/namelist"

if(len(sys.argv) != 2):
    print("USAGE: ./createGOADB.py fontfile")
    sys.exit()

font = fontforge.open(sys.argv[1])
font.saveNamelist(NAMELIST)
font.close()

f = file(NAMELIST)
baselist = f.readlines()
f.close()

newlist = []

for l in baselist:
    letter = l.strip()
    uczone = letter.split(" ")[0]
    lname = letter.split(" ")[1]
    ucode = "u"+uczone.split("0x")[1]
    newletter = lname + "\t" + lname + "\t" + ucode
    newlist.append(newletter+"\n")

f = file("GOADB.txt", 'w')
f.writelines(newlist)
f.close()
