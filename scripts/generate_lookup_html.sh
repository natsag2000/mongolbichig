#!/bin/tcsh
#  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
# | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
# | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
#
# create web page
#
# ХЭРЭГЛЭЭ:
#    ./generate_lookup_html.sh ../temp ../temp/png
#
# /tmp/result.html
# /tmp/glyphs.html үүсэх болно

# ===============================================

set sourcedir=$1
set glyphdir=$2
set OF=/tmp/whole.tmp
set FF=/tmp/features.tmp
set HF=/tmp/result.html
set GF=/tmp/glyphs.html

set CFilesBody=/tmp/classesbody.tmp
set CFilesHead=/tmp/classeshead.tmp
set GFilesBody=/tmp/groupsbody.tmp
set GFilesHead=/tmp/groupshead.tmp

echo "" > $HF
echo "" > $FF
echo "" > $OF
echo "" > $CFilesBody
echo "" > $CFilesHead
echo "" > $GFilesBody
echo "" > $GFilesHead

cat ./htmlhead > $HF

## feature files
echo '<a name="top">FEATURES</a><br/>' >> $HF
foreach F (`find $sourcedir -type f -name "*.th" | grep -v classes | grep -v group`)
  set fname=`basename $F | sed 's/.th//'`
  echo '<a href="#'$fname'">'$fname"</a>" >> $FF
  echo '<a name="'$fname'"><h1>'$fname'</h1></a> <a href="#top">top</a></br>' >> $OF
  cat $F | sed 's/\.\.\/png/png/g' >> $OF
end

## classes files
echo '<br/><br/><a>CLASSES</a><br/>' > $CFilesHead
foreach F (`find $sourcedir -type f -name "*.th" | grep classes`)
  set fname=`basename $F | sed 's/.th//'`
  echo '<a href="#CC'$fname'">@'$fname"</a>" >> $CFilesHead
  echo '<a name="CC'$fname'"><h1>@'$fname'</h1></a> <a href="#top">top</a></br>' >> $CFilesBody
  cat $F | sed 's/\.\.\/png/png/g' >> $CFilesBody
end

## group files
echo '<br/><br/><a>GROUPS</a><br/>' > $GFilesHead
foreach F (`find $sourcedir -type f -name "*.th" | grep group`)
  set fname=`basename $F | sed 's/.th//'`
  echo '<a href="#'$fname'">'$fname"</a>" >> $GFilesHead
  echo '<a name="'$fname'"><h1>'$fname'</h1></a> <a href="#top">top</a></br>' >> $GFilesBody
  cat $F | sed 's/\.\.\/png/png/g' >> $GFilesBody
end

cat $FF >> $HF
cat $CFilesHead >> $HF
cat $GFilesHead >> $HF
cat $OF >> $HF
cat $CFilesBody >> $HF
cat $GFilesBody >> $HF

cat ./htmlfoot >> $HF

# дүрснүүдээс вэб файл үүсгэх
cat ./htmlhead > $GF
echo "<h1>GLYPHS</h1>" >> $GF
foreach F (`find $glyphdir -type f -name "*.png"`)
  set bname=`basename $F`
  set gname=`basename $F | sed 's/.png//'`
  echo $gname'<br/><img src="png/'$bname'"><br/><br/>' >> $GF
end
cat ./htmlfoot >> GF
