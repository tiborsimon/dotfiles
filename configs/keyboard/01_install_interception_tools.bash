#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_aur_packages interception-tools interception-caps2esc
