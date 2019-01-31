#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# Install ctags for the system.
# Install ctags for the system.
# Install fzy as the fuzzy search driver.
install_packages gvim ctags fzy
