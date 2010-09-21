#!/usr/bin/env python
#
# crazy font creator
#
import fontforge ConfigParser

# read config files
config = ConfigParser.RawConfigParser()
config.read("config.cfg")

# TODO: check if they exist!
MONGOL_USEG     = config.get('Font data', 'MONGOL_USEG')
MONGOL_USEG_EXT = config.get('Font data', 'MONGOL_USEG_EXT')
GLYPHS_DIR      = config.get('Font data', 'GLYPHS_DIR'
BASE_FONT       = config.get('Font data', 'BASE_FONT')
FEATURE_FILE    = config.get('Font data', 'FEATURE_FILE')

# internal use
TMP_FONT = '/tmp/temp.sfd'

#BLANK_FONT = '../resources/blank.sfd'

# private usage zone
PRIVATE_ZONE =hex(0xf300)

font1 = fontforge.open(BASE_FONT)
# Delta char has conflict by otf generation, so delete it
font1["Delta"].clear()
font1.generate(TMP_FONT)
font = fontforge.open(TMP_FONT)

f = file(MONGOL_USEG, 'r')
mongoluseg = f.readlines()
f.close()
f = file(MONGOL_USEG_EXT, 'r')
mongoluseg_ext = f.readlines()
f.close()

# first unicode standart zone
# TODO check special cases: maaglha, dottedcircle etc!!
for letter in mongoluseg:
    letter = letter.strip()
    unizone=letter.split("uni")[1]
    ucode=int(unizone,16)
    # make new glyph
    #font.createMappedChar(letter) ##not works!
    font.createChar(ucode,letter)
    # import outline file
    # notice that font[glyphname] returns the appropriate glyph
    fontpath = GLYPHS_DIR + '/' +letter+".svg"
    font[letter].importOutlines(fontpath)
    # TODO same spacing for each letter, this is a hack after all TODO!!
    font[letter].left_side_bearing = 15
    font[letter].right_side_bearing = 15

# private zone
deccounter=int(PRIVATE_ZONE, 16)
for letter in mongoluseg_ext:
    letter = letter.strip()
    font.createChar(deccounter, letter)
    fontpath = GLYPHS_DIR + '/' +letter+".svg"
    print(fontpath)
    font[letter].importOutlines(fontpath)
    font[letter].left_side_bearing = -15
    font[letter].right_side_bearing = -15
    deccounter = deccounter + 1

# merge feature
font.mergeFeature(FEATURE_FILE)
# generate font
font.generate('crazy.ttf')
