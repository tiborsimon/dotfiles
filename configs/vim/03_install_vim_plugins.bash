#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

info "Installing VimPlug plugin manager.."
run curl -fLo ${HOME}/.vim/autoload/plug.vim --create-dirs \
	https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

info "Installing vim plugins.. (this could take a while..)"
run vim +PlugInstall +qall
