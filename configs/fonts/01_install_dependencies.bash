#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages ttf-dejavu ttf-liberation noto-fonts
install_aur_packages nerd-fonts-source-code-pro
