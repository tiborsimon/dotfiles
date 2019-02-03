#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_packages ttf-dejavu ttf-liberation
install_aur_packages nerd-fonts-source-code-pro
