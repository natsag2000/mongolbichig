#!/usr/bin/env python
#  __  __  ___  _   _  ____  ___  _       _   _ ____  _____ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |     | | | / ___|| ____/ ___|
# | |\/| | | | |  \| | |  _| | | | |     | | | \___ \|  _|| |  _
# | |  | | |_| | |\  | |_| | |_| | |___  | |_| |___) | |__| |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|  \___/|____/|_____\____|
#
# crazy font creator
#

import fontforge
import ConfigParser
import os

# GLOBALS
MONGOL_USEG      = ''
MONGOL_USEG_EXT  = ''
GLYPHS_DIR       = ''
BASE_FONT        = ''
FEATURE_FILE     = ''
FONTNAME         = ''
FAMILYNAME       = ''
FULLNAME         = ''
VERSION          = ''
FONT             = ''
# internal use
TMP_FONT = '/tmp/temp.sfd'
#BLANK_FONT = '../resources/blank.sfd'
# private usage zone
PRIVATE_ZONE =hex(0xf300)

def main():
    global MONGOL_USEG
    global MONGOL_USEG_EXT
    read_config()
    mongoluseg = read_lines(MONGOL_USEG)
    mongoluseg_ext = read_lines(MONGOL_USEG_EXT)
    prepare_font()
    create_standart_zone(mongoluseg)
    create_private_zone(mongoluseg_ext)
    replace_feature()
    generate_font("crazyfont.ttf")

def prepare_font():
    global BASE_FONT
    global TMP_FONT
    global FONT
    fontbase = fontforge.open(BASE_FONT)
    # Delta char has conflict by otf generation, so delete it
    fontbase["Delta"].clear()
    fontbase.generate(TMP_FONT)
    FONT = fontforge.open(TMP_FONT)

def read_config():
    global MONGOL_USEG
    global MONGOL_USEG_EXT
    global GLYPHS_DIR
    global BASE_FONT
    global FEATURE_FILE
    global FONTNAME
    global FAMILYNAME
    global FULLNAME
    global VERSION
    # read config files
    config = ConfigParser.RawConfigParser()
    config.read("config.cfg")
    # TODO: check if they exist!
    MONGOL_USEG      = config.get('Font creator', 'MONGOL_USEG')
    MONGOL_USEG_EXT  = config.get('Font creator', 'MONGOL_USEG_EXT')
    GLYPHS_DIR       = config.get('Font creator', 'GLYPHS_DIR')
    BASE_FONT        = config.get('Font creator', 'BASE_FONT')
    FEATURE_FILE     = config.get('Font creator', 'FEATURE_FILE')
    FONTNAME         = config.get('General', 'FONTNAME')
    FAMILYNAME       = config.get('General', 'FAMILYNAME')
    FULLNAME         = config.get('General', 'FULLNAME')
    VERSION          = config.get('General', 'VERSION')

def read_lines(FileName):
    f = file(FileName, 'r')
    lines = f.readlines()
    f.close()
    return lines

# first unicode standart zone
# TODO check special cases: maaglha, dottedcircle etc!!
def create_standart_zone(Usegs):
    global GLYPHS_DIR
    global FONT
    for letter in Usegs:
        letter = letter.strip()
        unizone=letter.split("uni")[1]
        ucode=int(unizone,16)
        FONT.createChar(ucode,letter)
        # import outline file
        # notice that font[glyphname] returns the appropriate glyph
        fontpath = GLYPHS_DIR + '/' +letter+".svg"
        FONT[letter].importOutlines(fontpath)
        # TODO same spacing for each letter, this is a hack after all TODO!!
        FONT[letter].left_side_bearing = 15
        FONT[letter].right_side_bearing = 15

def create_private_zone(Usegs):
    global PRIVATE_ZONE
    global GLYPHS_DIR
    global FONT
    deccounter=int(PRIVATE_ZONE, 16)
    for letter in Usegs:
        letter = letter.strip()
        FONT.createChar(deccounter, letter)
        fontpath = GLYPHS_DIR + '/' +letter+".svg"
        if not os.path.exists(fontpath):
            print "Glyph image file not found: ", fontpath
            continue
        FONT[letter].importOutlines(fontpath)
        FONT[letter].left_side_bearing = -15
        FONT[letter].right_side_bearing = -15
        deccounter = deccounter + 1

# replace feature
def replace_feature(Fea = None):
    global FONT
    global FEATURE_FILE
    Feature = None
    if Fea is None:
        Feature = FEATURE_FILE
    else:
        Feature = Fea
    for F in FONT.gsub_lookups:
        FONT.removeLookup(F)
    if not os.path.exists(Feature):
        print "Feature file is not found: ", Feature
    else:
        FONT.mergeFeature(Feature)

def generate_font(FontFileName):
    global FONTNAME
    global FAMILYNAME
    global FULLNAME
    global VERSION
    global FONT
    FONT.fullname = FULLNAME
    FONT.familyname = FAMILYNAME
    FONT.version = VERSION
    FONT.fontname = FONTNAME
    FONT.generate(FontFileName)

if __name__ == '__main__':
    main()
