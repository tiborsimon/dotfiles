#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/lib/libdeploy.bash

link_package vim \
             ./config/vimrc ${HOME}/.vimrc \
             ./config/vim ${HOME}/.vim

VIM_SPEEDDATING_CUSTOM_PATH="${HOME}/.vim/plugged/vim-speeddating/after/plugin"
mkdir -p ${VIM_SPEEDDATING_CUSTOM_PATH}

link_package vim \
             ./config/speeddating.vim ${VIM_SPEEDDATING_CUSTOM_PATH}/speeddating.vim
