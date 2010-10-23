#!/usr/bin/env python
#
# create list of all glyphs
# ============================

import sys
import fontforge
import os
import subprocess

MIN_MGL_CHAR   = int(hex(0x1800), 16)
MAX_MGL_CHAR   = int(hex(0x18A9), 16)
PRIVATE_MIN    = int(hex(0xE000), 16)
PRIVATE_MAX    = int(hex(0xF8FF), 16)
SPECIAL_MIN    = int(hex(0xFFF0), 16)
SPECIAL_MAX    = int(hex(0xFFFF), 16)
VARIATION_MIN  = int(hex(0xFE00), 16)
VARIATION_MAX  = int(hex(0xFE0F), 16)

def main():
    check_args()
    FontName = sys.argv[1]
    OutputFile = sys.argv[2]
    names = create_glyph_list(FontName)
    F = open(OutputFile, 'w')
    F.writelines(names)
    F.close()
    print 'Done'

def create_glyph_list(FontName):
    """
    list in the range of
    """
    names = []
    font = fontforge.open(FontName)
    iterator = font.glyphs()
    try:
        while 1:
            G = iterator.next()
            if not isInZone(G.unicode):
                continue
            #names.append(str(hex(G.unicode)) + ' : ' + G.glyphname)
            names.append(G.glyphname + '\n')
    except StopIteration:
        pass
    font.close()
    return names

def isInZone(Char):
    global MIN_MGL_CHAR
    global MAX_MGL_CHAR
    global PRIVATE_MIN
    global PRIVATE_MAX
    global SPECIAL_MIN
    global SPECIAL_MAX
    global VARIATION_MIN
    global VARIATION_MAX
    return MIN_MGL_CHAR <= Char <= MAX_MGL_CHAR or \
        PRIVATE_MIN <= Char <= PRIVATE_MAX or \
        SPECIAL_MIN <= Char <= SPECIAL_MAX or \
        VARIATION_MIN <= Char <= VARIATION_MAX


def check_args():
    if not len(sys.argv) == 3:
        print "\nUSAGE: " + sys.argv[0] + ' Font.otf ListFileName\n'
        sys.exit(1)
    if not os.path.exists(sys.argv[1]):
        print 'Font not found: ' + sys.argv[1]
        sys.exit(1)

if __name__ == '__main__':
    main()
