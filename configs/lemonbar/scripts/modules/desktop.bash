#!/usr/bin/env bash

source $1

TYPE=1

CURRENT=$(bspc query -D -d --names)

# Nerd Fonts from "f8a3" to "f8bd"
ICONS="..|..|..|..|..|..|..|..|.."

ICON=$(echo ${ICONS} | cut -d'|' -f ${CURRENT} | cut -d. -f ${TYPE})
echo -en "${FONT_4}${ICON}${FONT_1}"
