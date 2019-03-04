#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_packages emacs adobe-source-code-pro-fonts
install_packages poppler poppler-glib adobe-source-code-pro-fonts

# https://github.com/syl20bnr/spacemacs/issues/1012
install_aur_packages ttf-unifont
