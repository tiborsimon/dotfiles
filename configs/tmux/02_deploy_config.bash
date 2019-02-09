#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/tmux.conf ${HOME}/.tmux.conf

link_scripts tmux \
             ./scripts/init.bash
