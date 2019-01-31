#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

mkdir -p ${HOME}/.config

link_package ranger \
             ./config ${HOME}/.config/ranger
