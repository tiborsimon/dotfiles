#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/common.bash

mkdir -p ${HOME}/.config/fish

link_package fish \
             ./config/config.fish ${HOME}/.config/fish/config.fish
