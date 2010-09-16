#!/usr/bin/env python
#
# lista - listb = listc
#
import sys
if(len(sys.argv) != 3):
    print("USAGE: ./substractlist.py listAfile listBfile")
    sys.exit()

f = file(sys.argv[1], 'r')
listA = f.readlines()
f.close()
f = file(sys.argv[2], 'r')
listB = f.readlines()
f.close()

listC = list(set(listA).difference(listB))
f=file("substracted.txt", 'w')
f.writelines(listC)
f.close()


