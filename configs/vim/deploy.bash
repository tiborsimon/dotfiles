#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/lib/libdeploy.bash

link_package vim \
             ./config/vimrc ${HOME}/.vimrc \
             ./config/vim ${HOME}/.vim
