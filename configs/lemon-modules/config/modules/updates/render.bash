#!/usr/bin/env bash

UPDATE_ICON=""

updates=$(my-machine-pacman check --count-only --no-lemonbar --query-only)

if (( $updates > 0 ))
then
  echo -en "%{T1}%{A:updates:}%{T3}${UPDATE_ICON}%{T1}${updates}%{A}"
fi
