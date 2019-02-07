#!/usr/bin/env bash

NORMAL_FONT=$1
ICON_FONT=$2

CLOCK=$(date "+%F %A %R")

echo -en "%{A:clock:}${CLOCK}%{A}"
