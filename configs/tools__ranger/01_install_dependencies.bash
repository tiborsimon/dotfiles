#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages ranger

# Pillow needed for the kitty based image previews.
install_packages python-pillow
