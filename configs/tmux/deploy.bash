#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/lib/libdeploy.bash
mkdir -p ${HOME}/.local/bin

link_package tmux \
             ./config/tmux.conf ${HOME}/.tmux.conf

link_scripts tmux \
             ./scripts/init.bash
