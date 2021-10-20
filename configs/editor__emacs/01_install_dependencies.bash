#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages bdf-unifont
# To be able to run natively on wayland.
install_aur_packages emacs-gcc-wayland-devel-bin
