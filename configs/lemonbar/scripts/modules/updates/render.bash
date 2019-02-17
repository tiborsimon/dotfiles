#!/usr/bin/env bash

UPDATE_ICON=""
UPDATE_ICON=""

# updates=$(pacman -Qu | wc -l)
updates=3


if (( $updates > 0 ))
then
  echo -en "%{T1}%{A:updates:}%{T5}${UPDATE_ICON}%{T1} ${updates}%{A}"
else
  echo -en ""
fi
