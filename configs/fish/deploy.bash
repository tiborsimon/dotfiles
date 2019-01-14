#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/lib/libdeploy.bash

link_package fish \
             ./config/fish ${HOME}/.config/fish \
             ./config/omf ${HOME}/.config/omf
