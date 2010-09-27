#!/usr/bin/env python
#
# crazy font creator
#
import ConfigParser


def main():
    # read config files
    config = ConfigParser.RawConfigParser()
    config.read("config.cfg")
    # TODO: check if they exist!
    USEG_ATOM = config.get('Useg zurah', 'USEG_ATOM')
    USEG_DESC = config.get('Useg zurah', 'USEG_DESC')
    GLYPH_OUT = config.get('Useg zurah', 'GLYPH_OUT')
    # read atom and desc
    usegatoms = readlines(USEG_ATOM)
    usegdesc = readlines(USEG_DESC)
    print(usegatoms)

def readlines(FileName):
    f = file(FileName, 'r')
    lines = f.readlines()
    f.close()
    return lines

if __name__=='__main__':
    main()
