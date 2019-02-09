#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/fish ${HOME}/.config/fish \
  ./config/omf ${HOME}/.config/omf
