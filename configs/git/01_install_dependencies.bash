#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages git openssh

# Install fzy as the fuzzy search driver.
install_packages fzy
