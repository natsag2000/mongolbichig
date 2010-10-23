#!/bin/tcsh

# creates png glyph image from glyph name
# $1 = input folder
# $2 = output folder
# =============================================
set infolder=$1
set outputfolder=$2
set glyphtype=$3

if ( ! -d $outputfolder ) then
    mkdir $outputfolder
endif

foreach File (`find $infolder -type f -name "*.$glyphtype"`)
  set newname=`basename $File`
  convert -rotate -90 $File $outputfolder/$newname
end
