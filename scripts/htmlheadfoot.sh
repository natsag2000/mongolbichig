#!/bin/tcsh
#  __  __  ___  _   _  ____  ___  _     ____ ___ ____ _   _ ___ ____
# |  \/  |/ _ \| \ | |/ ___|/ _ \| |   | __ )_ _/ ___| | | |_ _/ ___|
# | |\/| | | | |  \| | |  _| | | | |   |  _ \| | |   | |_| || | |  _
# | |  | | |_| | |\  | |_| | |_| | |___| |_) | | |___|  _  || | |_| |
# |_|  |_|\___/|_| \_|\____|\___/|_____|____/___\____|_| |_|___\____|
#
# make html header and footer
# =============================================
set foldername=$1
foreach F (`find $foldername -type f -name "*.th"`)
    set newname=`echo $F | sed 's/.th/.html/'`
    cat htmlhead > $newname
    cat $F >> $newname
    cat htmlfoot >> $newname
end
