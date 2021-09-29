#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages neovim
install_packages fzy ripgrep

# info "Installing VimPlug plugin manager.."
# execute curl --fail --location --create-dirs \
#   --output "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim \
#   https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
