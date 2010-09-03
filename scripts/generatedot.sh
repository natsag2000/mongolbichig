#!/bin/tcsh
#  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
# | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
# | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
#
# create images from dot files using graphviz dot
# АНХААР:
#    өгсөн параметрт ямар ч шалгуур хийгээгүй
#    байгаа тул зөв сангуудыг өгч хэрэглэнэ үү!
# Хийх зүйлс:
#   - параметр шалгах
#   - хэрэглэх заавар харуулах
# ===============================================
set dotfolder=$1
set imagefolder=$2

if ( ! -d $imagefolder ) then
    mkdir $imagefolder
endif

foreach F (`find $dotfolder -type f -name "*.dot"`)
    set imagename=`basename $F | sed 's/.dot/.png/'`
    dot -Tpng $F > $imagefolder/$imagename
end
