#!/usr/bin/env python
#
# crazy font creator
#

MONGOL_USEG = '../misc/mongoluseg'
MONGOL_USEG_EXT='../misc/mongoluseg_ext'
GLYPHS_DIR = '/tmp/svgs'
BLANK_FONT = '../resources/blank.sfd'
BASE_FONT = '../resources/DejaVuSans.sfd'
TMP_FONT = '/tmp/temp.sfd'

# private zone
PRIVATE_ZONE =hex(0xf300)
import fontforge

font1 = fontforge.open(BASE_FONT)
# has conflict by otf generation
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
    font[letter].left_side_bearing = 15
    font[letter].right_side_bearing = 15
    deccounter = deccounter + 1

font.generate('crazy.ttf')
