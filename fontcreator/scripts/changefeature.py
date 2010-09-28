#!/usr/bin/env python
# changes feature in the otf font!
# arg1 => font
# arg2 => feature_file
# arg3 => result_font.ttf

import fontforge
import sys

TMP_FONT = "/tmp/temp.sfd"

def main():
    if len(sys.argv) != 4:
        usage()
        sys.exit(1)
    featureChange(sys.argv[1], sys.argv[2], sys.argv[3])

def featureChange(Font, Feature, ResultFont):
    font1 = fontforge.open(Font)
    font1.save(TMP_FONT)
    font = fontforge.open(TMP_FONT)
    for l in font.gsub_lookups:
        font.removeLookup(l)
    font.mergeFeature(Feature)
    font.generate(ResultFont)

def usage():
    print "\nUSAGE: ", sys.argv[0], ' font_file feature_file result_file.ttf\n'

if __name__ == '__main__':
    main()
