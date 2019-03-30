#!/usr/bin/env bash

OUTPUT=$1

CLOCK=$(date "+%F %A %R")

echo -en "%{T1}%{A:clock:}${CLOCK}%{A}" > $OUTPUT
