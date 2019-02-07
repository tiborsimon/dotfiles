#!/usr/bin/env bash

NORMAL_FONT=$1
ICON_FONT=$2

BATTERY_1_ICON='\uf244'
BATTERY_2_ICON='\uf242'

echo -en "${ICON_FONT}%{A:battery:}${BATTERY_1_ICON} ${BATTERY_2_ICON}%{A}${NORMAL_FONT}"
