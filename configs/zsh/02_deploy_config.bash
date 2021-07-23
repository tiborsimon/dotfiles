#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/zprofile' "${HOME}/.zprofile" \
  './config/zshrc' "${HOME}/.config/zsh/.zshrc" \
  './config/my-p10k.zsh' "${HOME}/.config/zsh/my-p10k.zsh"
