#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/profile' "${HOME}/.config/shell/profile" \
  './config/aliasrc' "${HOME}/.config/shell/aliasrc" \
