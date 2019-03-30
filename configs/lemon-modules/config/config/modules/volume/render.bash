#!/usr/bin/env bash

OUTPUT=$1

UNMUTED="墳"
MUTED="婢"

current_volume=$(my-machine-volume current)

if my-machine-volume is_muted
then
  out="%{T3}${MUTED} %{T1}${current_volume}"
else
  out="%{T3}${UNMUTED} %{T1}${current_volume}"
fi

echo -en "%{A:volume:}${out}%{A}%{T1}" > $OUTPUT
