#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# Firejail integration, we are using firecfg so firefox is automatically added.
link_package \
  './firejail/firefox.local' "${HOME}/.config/firejail/firefox.local"
