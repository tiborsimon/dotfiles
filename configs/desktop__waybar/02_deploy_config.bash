#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/config' "${HOME}/.config/waybar/config" \
  './config/style.css' "${HOME}/.config/waybar/style.css"

link_scripts waybar \
  './scripts/ping.bash' \
  './scripts/firejail.bash'
