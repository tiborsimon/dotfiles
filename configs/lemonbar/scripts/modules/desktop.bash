#!/usr/bin/env bash

source $1

TYPE=1
ICONS=".."

current=$(echo "$(bspc query -D -d --names) - 1" | bc)

icon_set=$(echo ${ICONS} | cut -d'.' -f ${TYPE})
icon=${icon_set:${current}:1}

echo -en "${FONT_3}${icon}${FONT_1}"
