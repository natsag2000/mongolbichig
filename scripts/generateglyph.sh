#!/bin/tcsh
#  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
# | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
# | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
#
# creates glyph image from glyph name
# =============================================
set fontname=$1
set foldername=$2
set outputfolder=$3
set PWD=`pwd`
foreach F (`find $foldername -type f`)
  foreach G (`cat $F`)
    $PWD/exportglyph.pe $fontname $G
  end
end

if ( ! -d $outputfolder ) then
    mkdir $outputfolder
endif

foreach File (*.bmp)
  set newname=`basename $File | sed 's/\$//g' | sed 's/.bmp/.png/'`
  convert $File $outputfolder/$newname
  rm $File
end
