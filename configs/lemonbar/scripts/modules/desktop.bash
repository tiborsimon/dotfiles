#!/usr/bin/env bash

NORMAL_FONT=$1
ICON_FONT=$2

CURRENT_DESKTOP=$(bspc query -D -d --names)
echo -en "[${CURRENT_DESKTOP}]"
