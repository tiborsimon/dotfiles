#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# for backlight adjustments
install_packages xorg-xbacklight

# for acpi event monitoring
install_packages acpid

install_aur_packages lemonbar-xft-git
