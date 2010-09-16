#!/bin/tcsh
#  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
# | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
# | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
#
# creates glyph image from glyph name
# $1 = font path
# $2 = glyph names
# $3 = output folder
# =============================================
set fontname=$1
set glyphnames=$2
set outputfolder=$3
set PWD=`pwd`
foreach G (`cat $glyphnames`)
    $PWD/export_svg_glyph.pe $fontname $G
end

if ( ! -d $outputfolder ) then
    mkdir $outputfolder
endif

foreach File (*.svg)
  set newname=`basename $File | sed 's/\$//g'`
  mv $File $outputfolder/$newname
end
