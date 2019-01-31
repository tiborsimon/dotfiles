#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package vim \
             ./config/vimrc ${HOME}/.vimrc

link_scripts vim \
             ./scripts/bookmarks.py

