#!/bin/tcsh
#  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
# | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
# | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
#
# санд буй зурагнуудын хэмжээг өөрчлөх

# convert -rotate 90 -transparent white -resize 30% glyph.png glyph.png
# ===============================================
set imagefolder=$1
mkdir /tmp/resized1
foreach F (`find $imagefolder -type f -name "*.png"`)
    set imagename=`basename $F`
    convert -rotate 90 -transparent white -resize 50% $F /tmp/resized1/$imagename
end
