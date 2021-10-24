#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/init.el' "${HOME}/.config/emacs/init.el" \
  './config/conf.org' "${HOME}/.config/emacs/conf.org"
