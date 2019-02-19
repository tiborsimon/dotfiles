#!/usr/bin/env bash

cd $(dirname $(readlink -f $0))

source ./config.bash

echo "checking for updates.."

sudo pacman --sync --refresh --refresh --sysupgrade --downloadonly --noconfirm &>/dev/null

updates=$(pacman -Qu | wc -l)
echo "$updates were found."

echo $updates > $TEMP_FILE

echo "checking for updates finished"
