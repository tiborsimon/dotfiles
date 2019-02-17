#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_scripts machine \
  ./scripts/volume.bash \
  ./scripts/brightness.bash
