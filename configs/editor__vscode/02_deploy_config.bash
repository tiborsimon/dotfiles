#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/snippets' "${HOME}/.config/Code/User/snippets" \
  './config/keybindings.json' "${HOME}/.config/Code/User/keybindings.json" \
  './config/settings.json' "${HOME}/.config/Code/User/settings.json" \
  './config/spellright.dict' "${HOME}/.config/Code/User/spellright.dict"

link_scripts code \
  './scripts/run.sh'
