#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# Install fzy as the fuzzy search driver.
install_packages fzy
