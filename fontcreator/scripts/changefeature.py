#!/usr/bin/env python
# changes feature in the otf font!
# arg1 => font
# arg2 => feature_file

import fontforge
import sys

TMP_FONT = "/tmp/temp.sfd"

def main():
    if len(sys.argv) != 3:
        usage()
        sys.exit(1)
    featureChange(sys.argv[1], sys.argv[2])

def featureChange(Font, Feature):
    font1 = fontforge.open(Font)
    font1.save(TMP_FONT)
    font = fontforge.open(TMP_FONT)
    for l in font.gsub_lookups:
        font.removeLookup(l)
    font.mergeFeature(Feature)
    font.generate('crazy_feature.ttf')

def usage():
    print "\nUSAGE: ", sys.argv[0], ' font_file feature_file\n'

if __name__ == '__main__':
    main()
