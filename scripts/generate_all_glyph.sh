#!/bin/tcsh
#  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
# | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
# | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
#
# create images from dot files using graphviz dot
#
# ХЭРЭГЛЭЭ:
#    ./generate_all_glyph.sh ../fonts/Mongolian_Baiti.ttf ../MongolianBaiti ../web/images
#
# АНХААР:
#    өгсөн параметрт ямар ч шалгуур хийгээгүй
#    байгаа тул зөв сангуудыг өгч хэрэглэнэ үү!
# Хийх зүйлс:
#   - параметр шалгах
#   - хэрэглэх заавар харуулах
# ===============================================
set fontname=$1
set fontfolder=$2
set glyphfolder=$3

set classfolder=$fontfolder/classes
set featurefolder=$fontfolder/features
set miscfolder=../misc

set classglyphs=$miscfolder/classglyphs
set featureglyphs=$miscfolder/featureglyphs
set replaceglyphs=$miscfolder/replaceglyphs
set allglyphs=$miscfolder/allglyphs

set tmpfile=/tmp/test

# first clean!
[ -f $classglyphs ] && rm $classglyphs
[ -f $featureglyphs ] && rm $featureglyphs
[ -f $replaceglyphs ] && rm $replaceglyphs
[ -f $allglyphs ] && rm $allglyphs
[ ! -d $miscfolder ] && mkdir $miscfolder

# create glyphs list from classfolder
# ===================================
foreach F (`find $classfolder -type f`)
    cat $F >> $tmpfile
end
cat $tmpfile | sort -u > $classglyphs

# create featureglyphs and replaceglyphs list from featurefolder
# ==============================================================
rm $tmpfile
foreach F (`find $featurefolder -type f`)
    cat $F >> $tmpfile
end
cat $tmpfile | grep uni | tr ' ' '\n' | grep uni | tr ';' ' ' | tr "'" ' ' | tr ']' ' ' | tr '[' ' ' | sed 's/ //g' | sort -u > $featureglyphs
cat $tmpfile | grep uni | grep by | sed 's/by/:/' | cut -d":" -f2 | tr ' ' '\n' | grep uni | tr ';' ' ' | tr "'" ' ' | tr ']' ' ' | tr '[' ' ' | sed 's/ //g' | sort -u > $replaceglyphs

# create all glyphs
# ==================
rm $tmpfile
cat $classglyphs >> $tmpfile
cat $featureglyphs >> $tmpfile
cat $tmpfile | sort -u > $allglyphs

# create glyphs from allglyphs
# ============================
if ( ! -d $glyphfolder ) then
    mkdir $glyphfolder
endif

foreach G (`cat $allglyphs`)
  $PWD/exportglyph.pe $fontname $G
end

foreach File (*.bmp)
  set newname=`basename $File | sed 's/\$//g' | sed 's/.bmp/.png/'`
  convert $File $glyphfolder/$newname
  rm $File
end
