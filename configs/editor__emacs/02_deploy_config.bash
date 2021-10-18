#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/init.el' "${HOME}/.emacs.d/init.el" \
  './config/conf.org' "${HOME}/.emacs.d/conf.org"
