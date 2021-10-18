#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config' "${HOME}/.config/nvim"


write_to_messages " nvim - You should run the PlugInstall command in neovim."
