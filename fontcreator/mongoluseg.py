#!/usr/bin/env python
#  __  __  ___  _   _  ____  ___  _       _   _ ____  _____ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |     | | | / ___|| ____/ ___|
# | |\/| | | | |  \| | |  _| | | | |     | | | \___ \|  _|| |  _
# | |  | | |_| | |\  | |_| | |_| | |___  | |_| |___) | |__| |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|  \___/|____/|_____\____|
#
# crazy useg creator
# Author: nagi (natsag2000@googlemail.com)
#
import ConfigParser
import subprocess
import re
import os

def main():
    # read config file
    config = ConfigParser.RawConfigParser()
    config.read("config.cfg")
    # TODO: check if they exist!
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

def useg_uusge(Atoms, Desc, AFolder, OFolder):
    atomkeys = Atoms.keys()
    for Name, Instr in Desc.items():
        li = []
        for I in splitAndStrip(Instr, ' '):
            if not I in atomkeys:
                if isNumber(I):
                    li.append("--margin "+I)
                    continue
                else:
                    print Name, ':', "Key not found: ", I
                    break
            if I == ' ' or I == '':
                continue
            li.append(AFolder+'/' + Atoms[I] + '.svg')
        Cmd = 'svg_stack.py ' + " --margin -10 ".join(li) + ' > ' + OFolder +'/'+Name + '.svg'
        Cmd1 = Cmd.split(' ')
        if not subprocess.call(Cmd, shell=True) == 0:
            print "Not created: " + Cmd
        else:
            print Name, " created succesfully!"

def isNumber(Str):
    if Str.startswith("-") or Str.startswith("+"):
        return Str[1:].isdigit()
    return Str.isdigit()

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

def splitAndStrip(Line, Pattern):
    return [ X.strip() for X in Line.split(Pattern) if X != '']

def isEmptyOrComment(Line):
    if Line.startswith('#') or len(Line) == 0:
        return True

def readlines(FileName):
    f = file(FileName, 'r')
    lines = f.readlines()
    f.close()
    return lines

if __name__=='__main__':
    main()
