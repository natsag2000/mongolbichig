#!/bin/tcsh

set sourcedir=$1
set outputfile=otfclasses

echo "" > $outputfile
foreach F (`find $sourcedir -type f`)
  set bname=`basename $F`
  echo "@"$bname"=[" | tr '\n' ' ' >> $outputfile
  cat $F | tr '\n' ' ' >> $outputfile
  echo "];" >> $outputfile
end
