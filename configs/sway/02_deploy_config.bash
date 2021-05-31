#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

SCREENSHOTS_DIR="${HOME}/screenshots"
if [ ! -d "$SCREENSHOTS_DIR" ]
then
  execute mkdir -v "$SCREENSHOTS_DIR"
fi

link_package \
  './config/config' "${HOME}/.config/sway/config" \
  './config/status.sh' "${HOME}/.config/sway/status.sh"

link_scripts sway \
  './scripts/scratchpad.bash' \
  './scripts/lock.bash' \
  './scripts/idle.bash' \

