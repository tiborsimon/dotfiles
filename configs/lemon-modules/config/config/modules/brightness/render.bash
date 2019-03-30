#!/usr/bin/env bash

OUTPUT=$1

LIGHT="ﯦ"

current_brightness=$(python -c "print(round($(xbacklight -get)))")

echo -en "%{A:brightness:}%{T3}${LIGHT} %{T1}${current_brightness}%{A}%{T1}" > $OUTPUT
