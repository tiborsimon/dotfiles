#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  ./config/local.conf /etc/fonts/local.conf \
  ./config/29-prettify.conf /etc/fonts/conf.avail/29-prettify.conf \
  /etc/fonts/conf.avail/29-prettify.conf /etc/fonts/conf.d/29-prettify.conf
