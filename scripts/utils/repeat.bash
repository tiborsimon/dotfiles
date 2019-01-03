#!/usr/bin/env bash

INTERVAL=$1
shift
START=$(date +%T)

BOLD=$(tput bold)
RESET=$(tput sgr0)

while true; do
  clear
  echo "${BOLD}MY_REPEAT${RESET} - Running ${BOLD}$@${RESET} in every $INTERVAL seconds.."
  echo "$START  - $(date +%T)"
  echo ""
  eval "$@"
  sleep $INTERVAL
done

