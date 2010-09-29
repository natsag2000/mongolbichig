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
    $PWD/export_svg_glyph.pe $fontname $G
end

if ( ! -d $outputfolder ) then
    mkdir $outputfolder
endif

foreach File (*.svg)
  set newname=`basename $File | sed 's/\$//g' | sed 's/.svg/.png/'`
  convert -rotate 90 -transparent white -resize 50% $File $outputfolder/$newname
  rm $File
end
