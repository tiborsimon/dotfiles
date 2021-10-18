#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# For pyenv building
install_packages pyenv

using pip
info 'Installing virtualenvwrapper..'
execute pip install --user virtualenvwrapper

# Installing pyenv..
#pyenv_base="${HOME}/.config/pyenv"
#git clone https://github.com/pyenv/pyenv.git "$pyenv_base"

#write_to_messages " pyenv - You need to manually go into '${pyenv_base}' and execute: 'src/configure && make -C src'"
