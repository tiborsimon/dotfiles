#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages neovim
install_packages fzy ripgrep fzf

# Typescript
execute npm -g install \
  typescript-language-server \
  diagnostic-languageserver \
  eslint \
  prettier \
  @typescript-eslint/parser \
  @typescript-eslint/eslint-plugin

# Python
execute npm -g install \
  pyright 
