#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/common.bash

link_package tmux \
             ./config/tmux.conf ${HOME}/.tmux.conf
