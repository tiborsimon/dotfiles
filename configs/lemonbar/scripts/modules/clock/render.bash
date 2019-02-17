#!/usr/bin/env bash

CLOCK=$(date "+%F %A %R")

echo -en "%{T1}%{A:clock:}${CLOCK}%{A}"
