#!/usr/bin/env bash

source $1

CLOCK=$(date "+%F %A %R")

echo -en "${FONT_1}%{A:clock:}${CLOCK}%{A}"
