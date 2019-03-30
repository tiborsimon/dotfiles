#!/usr/bin/env bash

OUTPUT=$1

UPDATE_ICON=""

updates=$(my-machine-pacman check --count-only --no-lemonbar)

if (( $updates > 0 ))
then
  echo -en "%{T1}%{A:updates:}%{T3}${UPDATE_ICON}%{T1}${updates}%{A}" > $OUTPUT
else
  echo "" > $OUTPUT
fi
