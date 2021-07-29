#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/config' "${HOME}/.config/bpytop/config"

# Firejail integration
link_package \
  './firejail/bpytop.profile' "${HOME}/.config/firejail/bpytop.profile"
