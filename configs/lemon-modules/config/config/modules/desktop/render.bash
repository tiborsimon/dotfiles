#!/usr/bin/env bash

OUTPUT=$1

TYPE=1
ICONS=".."

current=$(echo "$(bspc query -D -d --names) - 1" | bc)

icon_set=$(echo ${ICONS} | cut -d'.' -f ${TYPE})
icon=${icon_set:${current}:1}

echo -en "%{T3}${icon}%{T1}" > $OUTPUT
