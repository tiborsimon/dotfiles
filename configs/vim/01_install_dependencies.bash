#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# Install fzy as the fuzzy search driver.
# Install python pygments for syntax highlighting.
install_packages gvim fzy python-pygments

# Install ctags and gtags (global)
install_aur_packages universal-ctags-git global
