#!/bin/tcsh

# creates png glyph image from glyph name
# $1 = font path
# $2 = glyph name list
# $3 = output folder
# =============================================
set fontname=$1
set glyphlist=$2
set outputfolder=$3
set PWD=`pwd`
foreach G (`cat $glyphlist`)
    $PWD/exportglyph.pe $fontname $G
end

if ( ! -d $outputfolder ) then
    mkdir $outputfolder
endif

foreach File (*.bmp)
  set newname=`basename $File | sed 's/\$//g' | sed 's/.bmp/.png/'`
  convert $File $newname
  rm $File
end

foreach File (*.png)
  set newname = `basename $File`
  convert -rotate 90 -transparent white -resize 60% $File $outputfolder/$File
  #convert -transparent white -resize 60% $File $outputfolder/$File
  rm $File
end
