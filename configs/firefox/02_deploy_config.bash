#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# Firejail integration
link_package \
  './firejail/firefox.profile' "${HOME}/.config/firejail/firefox.profile" \
  './firejail/firefox.shim' "${HOME}/.local/bin/firefox"

