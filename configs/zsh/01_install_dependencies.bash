#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages zsh zsh-syntax-highlighting zsh-autosuggestions

powerlevel10k_repo_root="${HOME}/.config/zsh/powerlevel10k"

execute mkdir --parent --verbose "$(dirname "$powerlevel10k_repo_root")"

if [ ! -d "$powerlevel10k_repo_root" ]
then
  execute git clone \
    --depth 1 \
    --branch 'v1.15.0' \
    'https://github.com/romkatv/powerlevel10k.git' \
    "$powerlevel10k_repo_root"
fi
