#!/usr/bin/env bash

if [ ! -x "$(which convert)" ]; then
    echo "ImageMagick needed.."
    exit 1
fi;

# Convert RAW images to 2560px max-dimension JPEG @ 80 quality to ./output
# check for presence of convert command first:
quality=${1:-80};
max_dim=${2:-2650};
source_files=${3:-\*.CR2};
echo "Usage: convert-raw-to-jpg [quality=80] [max-dimension-px=2560] [source=\*.CR2]";
echo "Converting all ${source_files} to JPEG (${quality} quality, ${max_dim}px max) to output/...";
mkdir -p output 2> /dev/null;
find . -type f -iname "${source_files}" -print0 | \
    xargs -0 -n 1 -P 8 -I {} convert -verbose -units PixelsPerInch {} \
    -colorspace sRGB -resize ${max_dim}x${max_dim} -set filename:new '%t-%wx%h' \
    -density 72 -format JPG -quality ${quality} 'output/%[filename:new].jpg';
echo 'Done.';
