#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/config.lua' "${HOME}/.config/lvim/config.lua"

