#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_packages emacs

doom_emacs_base="${HOME}/.config/emacs"
if [ ! -d "$doom_emacs_base" ]
then
  git clone --depth 1 https://github.com/hlissner/doom-emacs "$doom_emacs_base"
fi

write_to_messages " doom-emacs - You need to manually execute the 'doom install' command."
