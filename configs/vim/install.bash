#!/usr/bin/env bash

# Install VimPlug to the unlinked local directory structure
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Install ctags for the system
sudo pacman -S ctags

# Install fzy as the fuzzy search driver
sudo pacman -S ctags
