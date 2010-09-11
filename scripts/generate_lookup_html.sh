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

echo "" > $HF
echo "" > $FF
echo "" > $OF

cat ./htmlhead > $HF
echo '<a name="top">FEATURES</a><br/>' >> $HF
foreach F (`find $sourcedir -type f -name "*.th"`)
  set fname=`basename $F | sed 's/.th//'`
  echo '<a href="#'$fname'">'$fname"</a>" >> $FF
  echo '<a name="'$fname'"><h1>'$fname'</h1></a> <a href="#top">top</a></br>' >> $OF
  cat $F | sed 's/\.\.\/png/png/g' >> $OF
end

cat $FF >> $HF
cat $OF >> $HF

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