#!/usr/bin/env bash

cd $(dirname $(readlink -f $0))

source ./config.bash

UPDATE_ICON=""

updates=$(cat $TEMP_FILE 2>/dev/null || echo 0)

if (( $updates > 0 ))
then
  echo -en "%{T1}%{A:updates:}%{T3}${UPDATE_ICON}%{T1}${updates}%{A}"
fi
