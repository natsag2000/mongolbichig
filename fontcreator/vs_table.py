#!/usr/bin/env python
#  __  __  ___  _   _  ____  ___  _       _   _ ____  _____ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |     | | | / ___|| ____/ ___|
# | |\/| | | | |  \| | |  _| | | | |     | | | \___ \|  _|| |  _
# | |  | | |_| | |\  | |_| | |_| | |___  | |_| |___) | |__| |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|  \___/|____/|_____\____|
#
# vs table generator
# Author: nagi (natsag2000@googlemail.com)
#

import ConfigParser
import os
import sys

# GLOBALS
V1_FOLDER      = ''
V2_FOLDER      = ''
MGL_LETTERS      = ''

def main(Filename):
    global V1_FOLDER
    global V2_FOLDER
    global MGL_LETTERS
    html = []
    read_config()
    mongoluseg = read_lines(MGL_LETTERS)
    F = open(Filename, 'w')
    F.write(get_html_head())
    F.writelines(make_table(V1_FOLDER, V2_FOLDER, mongoluseg))
    F.write(get_html_foot())
    F.close()

def make_table(f1, f2, letters):
    t = []
    for l in letters:
        l = l.strip()
        l = l.replace("\n", '')
        t.append('<tr><td><img src='+f1+'/'+l+'.png></td><td><img src='+f2+'/'+l+'.png></td><td>'+l+'</td></tr>\n')
    return t

def get_html_foot():
    return '</table></body></html>'

def get_html_head():
    h = '<head> \
<meta http-equiv="Content-Type" content="text/html; \
charset=UTF-8" /> \
</head> \
<body> \
<h1> \
<table>'
    return h

def read_config():
    global V1_FOLDER
    global V2_FOLDER
    global MGL_LETTERS
    # read config files
    config = ConfigParser.RawConfigParser()
    config.read("config.cfg")
    # TODO: check if they exist!
    V1_FOLDER      = config.get('VS TABLE', 'V1_FOLDER')
    V2_FOLDER      = config.get('VS TABLE', 'V2_FOLDER')
    MGL_LETTERS    = config.get('VS TABLE', 'MGL_LETTERS')

def read_lines(FileName):
    f = file(FileName, 'r')
    lines = f.readlines()
    f.close()
    return lines

def usage():
    print "USAGE: " + sys.argv[0] + " html_file_name"
    sys.exit()

if __name__ == '__main__':
    if len(sys.argv) != 2:
        usage()
    main(sys.argv[1])
