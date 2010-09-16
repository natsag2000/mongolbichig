#!/usr/bin/env python
#
# only < uni1842 letters
#
import sys
if(len(sys.argv) != 2):
    print("USAGE: ./onlymglletter.py listfile")
    sys.exit()

def checkCode(code):
    if int(PATTERN_LETTER, 16) >= int(code,16):
        return True
    return False

PATTERN_LETTER = hex(0x1842)

f = file(sys.argv[1], 'r')
allletter = f.readlines()
f.close()
newlist = []

for l in allletter:
    letter = l.strip()
    letter = letter.strip()
    if (not letter.startswith("uni")):
        continue
    letter = letter.split("uni")[1]
    codes = letter.split(".")[0]
    if (len(codes) == 4):
        if (checkCode(codes)):
            newlist.append(l)
    elif(checkCode(codes[:4]) and checkCode(codes[4:8]) ):
        newlist.append(l)

f = file("mglletters.txt", 'w')
f.writelines(newlist)
f.close()
