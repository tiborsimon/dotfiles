#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_config ./config/tmux.conf ${HOME}/.tmux.conf

link_script ./scripts/init.bash my-tmux-init
