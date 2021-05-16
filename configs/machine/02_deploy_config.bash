#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_scripts machine \
  './scripts/volume.bash' \
  './scripts/pacman.bash'
