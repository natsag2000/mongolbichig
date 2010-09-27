#!/usr/bin/env python
#
# crazy font creator
#
import ConfigParser
import re

def main():
    # read config file
    config = ConfigParser.RawConfigParser()
    config.read("config.cfg")
    # TODO: check if they exist!
    USEG_ATOM = config.get('Useg zurah', 'USEG_ATOM')
    USEG_DESC = config.get('Useg zurah', 'USEG_DESC')
    ATOM_FOLDER=config.get('Useg zurah', 'ATOM_FOLDER')
    GLYPH_OUT = config.get('Useg zurah', 'GLYPH_OUT')
    # read atom and desc
    usegatoms = readlines(USEG_ATOM)
    usegdesc = readlines(USEG_DESC)
    atoms = read_atoms(usegatoms)
    descs = read_desc(usegdesc)
    #for k, v in descs.items():
    #    print k, '  =  ', v
    useg_uusge(atoms, descs, ATOM_FOLDER)

# TODO: check file if exits!
def useg_uusge(Atoms, Desc, AFolder):
    atomkeys = Atoms.keys()
    for Name, Instr in Desc.items():
        li = []
        for I in splitAndStrip(Instr, ' '):
            if not I in atomkeys:
                print Name, ':', "Key not found: ", I
                break
            if l == ' ':
                continue
            li.append(AFolder+'/' + Atoms[I] + '.svg')
        Cmd = 'pythonx ' + " ".join(li) + ' > ' + Name + '.svg'
        print Cmd

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
    return [ X.strip() for X in Line.split(Pattern)]

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
