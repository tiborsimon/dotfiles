#!/usr/bin/env bash

LIGHT="ﯦ"

current_brightness=$(my-machine-brightness current)

echo -en "%{A:brightness:}%{T3}${LIGHT} %{T1}${current_brightness}%{A}%{T1}"
