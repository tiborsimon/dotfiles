#!/usr/bin/env bash

for file in $(find . \( -name "*.mp4" -o -name "*.MP4" -o -name "*.mov" -o -name "*.MOV" \)); do
    printf "Processing ${file}.."
    mv ${file} "${file}_original"
    ffmpeg -loglevel panic -i "${file}_original" -c:v libx264 -crf 28 -c:a aac -q:a 70 -map_metadata 0 ${file}
    exiftool -q '-datetimeoriginal<createdate' -if 'not $datetimeoriginal or ($datetimeoriginal eq "0000:00:00 00:00:00")' ${file}
    rm "${file}_original"
    printf "\rProcessing ${file}.. Done\n"
done



