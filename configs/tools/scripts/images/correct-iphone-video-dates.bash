#!/usr/bin/env bash

direction=${1:-+}
amount=${2:-1}

echo "Direction: ${direction}"
echo "Amount: ${amount}"

if [ "$direction" == "+" ]; then
    exiftool -alldates+=$amount -if '$filetype eq "MOV"' .
elif [ "$direction" == "-" ]; then
    exiftool -alldates-=$amount  -if '$filetype eq "MOV"' .
else
    echo "Invalid input argument! [+|-]"
    exit 1
fi
rm *_original
images-rename
