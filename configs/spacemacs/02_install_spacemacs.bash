#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

function install_spacemacs {
  if [ ! -d ${HOME}/.emacs.d ]; then
    git clone https://github.com/syl20bnr/spacemacs ${HOME}/.emacs.d
  fi
}

execute install_spacemacs
