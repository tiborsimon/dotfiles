#!/usr/bin/env bash

# weather=$(curl wttr.in?format="+%t")
weather="3%{T2}%{T1}"

echo -en "%{T1}%{A:weather:}${weather}%{A}"
