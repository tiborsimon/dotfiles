#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/lib/libdeploy.bash
mkdir -p ${HOME}/.local/bin

link_package tmux \
             ./config/tmux.conf ${HOME}/.tmux.conf \
             ./scripts/init.bash ${HOME}/.local/bin/my-tmux-init
