#!/usr/bin/env python
# -- coding: utf-8 --
#@+leo-ver=5-thin
#@+node:nagi.20101207083602.1297: * @file {{os.getenv('HOME')}}/nagi-gits/mongolbichig/fontcreator/mongoluseg.py
#@@first
#@@first
#@@language python
#@@tabwidth -4
#@+others
#@+node:nagi.20101207083602.1309: ** толгой хэсэг
# ----------------------------------------------------------------
#  __  __  ___  _   _  ____  ___  _       _   _ ____  _____ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |     | | | / ___|| ____/ ___|
# | |\/| | | | |  \| | |  _| | | | |     | | | \___ \|  _|| |  _
# | |  | | |_| | |\  | |_| | |_| | |___  | |_| |___) | |__| |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|  \___/|____/|_____\____|
#
# crazy useg creator
# үсэг үүсгэгч
#
# Author: nagi (natsag2000@googlemail.com)
# ----------------------------------------------------------------
#@+node:nagi.20101207083602.1298: ** import хэсэг
import ConfigParser
import subprocess
import re
import os

#@+node:nagi.20101207083602.1299: ** main гол дуудагдах функц
def main():
    # read config file
    config = ConfigParser.RawConfigParser()
    config.read("config.cfg")
    #@+<<TODO тохиргооны оруулгууд зөв эсэхийг шалгах>>
    #@+node:nagi.20101207083602.1313: *3* <<TODO тохиргооны оруулгууд зөв эсэхийг шалгах>>
    # TODO: Тохиргооны оруулгууд зөв үү гэдгийг шалга!
    #@-<<TODO тохиргооны оруулгууд зөв эсэхийг шалгах>>
    USEG_ATOM = config.get('Mongol useg', 'USEG_ATOM')
    USEG_DESC = config.get('Mongol useg', 'USEG_DESC')
    ATOM_FOLDER=config.get('Mongol useg', 'ATOM_FOLDER')
    GLYPH_OUT = config.get('Mongol useg', 'GLYPH_OUT')
    if not os.path.exists(GLYPH_OUT):
        os.makedirs(GLYPH_OUT)
    # read atom and desc
    usegatoms = readlines(USEG_ATOM)
    usegdesc = readlines(USEG_DESC)
    atoms = read_atoms(usegatoms)
    descs = read_desc(usegdesc)
    useg_uusge(atoms, descs, ATOM_FOLDER,  GLYPH_OUT)

#@+node:nagi.20101207083602.1300: ** useg_uusge
def useg_uusge(Atoms, Desc, AFolder, OFolder):
    atomkeys = Atoms.keys()
    Missed = {}
    for Name, Instr in Desc.items():
        li = []
        NotCreated = []
        Stopped = False
        for I in splitAndStrip(Instr, ' '):
            if not I in atomkeys:
                if isNumber(I):
                    li.append("--margin "+I)
                    continue
                else:
                    Stopped = True
                    print Name, ':', "Key not found: ", I
                    break
            if I == ' ' or I == '':
                continue
            FilePath = AFolder+'/' + Atoms[I] + '.svg'
            if not os.path.exists(FilePath):
                NotCreated.append(FilePath)
                continue
            li.append(FilePath)
        if li == [] or Stopped == True:
            Missed[Name] = " is not created!"
            print Name, " is not created!"
            createByCopy(AFolder, OFolder, "soyombo.svg", Name + '.svg')
            continue
        if NotCreated != []:
            Missed[Name] = NotCreated
            #print Name, " is NOT CREATED: ", NotCreated
            createByCopy(AFolder, OFolder, "soyombo.svg", Name + '.svg')
            continue
        Cmd = 'svg_stack.py ' + " --margin -10 ".join(li) + ' > ' + OFolder +'/'+Name + '.svg'
        Cmd1 = Cmd.split(' ')
        if not subprocess.call(Cmd, shell=True) == 0:
            print "Not created: " + Cmd
        else:
            print Name, " created succesfully!"
    if Missed != {}:
        print 'NOT CREATED:'
        for Key in Missed.keys():
            print Key, ' : ', Missed[Key]

#@+node:nagi.20101207083602.1301: ** createByCopy
#@+at
# зөвхөн сангаас сангийн хооронд хуулна
#@@c
def createByCopy(AFolder, OFolder, NameA, NameB):
    AFile = AFolder + '/' + NameA
    BFile = OFolder + '/' + NameB
    copyGlyph(AFile, BFile)

#@+node:nagi.20101207083602.1302: ** copyGlyph
def copyGlyph(A, B):
    Cmd = 'cp ' + A + ' ' + B
    if not subprocess.call(Cmd, shell=True) == 0:
        print "Not Created: " + Cmd
    else:
        print B, " is copied succesfully!"

#@+node:nagi.20101207083602.1303: ** isNumber
def isNumber(Str):
    if Str.startswith("-") or Str.startswith("+"):
        return Str[1:].isdigit()
    return Str.isdigit()

#@+node:nagi.20101207083602.1304: ** read_atoms
def read_atoms(List):
    dic = dict()
    for L in List:
        L1 = L.strip()
        if isEmptyOrComment(L1):
            continue
        L1 = L.split('#')[0]
        L1 = L1.strip()
        m = re.search(' +', L1)
        if(m):
            kv = L1.split(m.group(0))
            dic[kv[0]] = kv[1]
        else:
            dic[L1] = L1
    return dic

#@+node:nagi.20101207083602.1305: ** read_desc
def read_desc(List):
    dic = dict()
    for L in List:
        L1 = L.strip()
        if isEmptyOrComment(L1):
            continue
        L2 = splitAndStrip(L1, '=')
        # if no desc, skip it
        if len(L2) == 1:
            print "Skipping: ", L2[0]
            continue
        dic[L2[0]] = L2[1]
    return dic

#@+node:nagi.20101207083602.1306: ** splitAndStrip
def splitAndStrip(Line, Pattern):
    return [ X.strip() for X in Line.split(Pattern) if X != '']

#@+node:nagi.20101207083602.1307: ** isEmptyOrComment
def isEmptyOrComment(Line):
    if Line.startswith('#') or len(Line) == 0:
        return True

#@+node:nagi.20101207083602.1308: ** readlines
def readlines(FileName):
    f = file(FileName, 'r')
    lines = f.readlines()
    f.close()
    return lines

#@-others
if __name__=='__main__':
    main()
#@-leo
