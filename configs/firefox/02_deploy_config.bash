#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# Firejail integration
link_package \
  './firejail/firefox.local' "${HOME}/.config/firejail/firefox.local" \
  './firejail/firefox.shim' "${HOME}/.local/bin/firefox"
