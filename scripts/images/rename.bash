#!/usr/bin/env bash

# Update missing DateOriginalDate if missing
# https://gist.github.com/rjames86/33b9af12548adf091a26
echo "Updating missing metadata.."
echo "FileModifyDate -> CreateDate"
exiftool '-createdate<filemodifydate' -if 'not $createdate or ($createdate eq "0000:00:00 00:00:00")' .
echo "CreateDate -> DateTimeOriginal"
exiftool '-datetimeoriginal<createdate' -if 'not $datetimeoriginal or ($datetimeoriginal eq "0000:00:00 00:00:00")' .
rm -f *_original

# Rename files
echo "Renaming files.."
exiftool -P -d "%Y%m%d-%H%M%S%%-c.%%e" '-FileName<DateTimeOriginal' .

# AVI support
# http://u88.n24.queensu.ca/exiftool/forum/index.php?topic=3061.0
if ls *.AVI > /dev/null 2>&1; then
    echo "Separate AVI processing.."
    echo "Updating missing metadata.."
    echo "FileModifyDate -> CreateDate"
    exiftool '-createdate<filemodifydate' -if 'not $createdate or ($createdate eq "0000:00:00 00:00:00")' -ext AVI .
    echo "CreateDate -> DateTimeOriginal"
    exiftool '-datetimeoriginal<createdate' -if 'not $datetimeoriginal or ($datetimeoriginal eq "0000:00:00 00:00:00")' -ext AVI .
    echo "Renaming files.."
    rm -f *_original
    exiftool -P -d "%Y%m%d-%H%M%S%%-c.%%e" '-FileName<DateTimeOriginal' -ext AVI .
fi

echo "Done."
