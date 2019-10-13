#! /usr/bin/env bash

clear
echo "start" > dotfiles_fifo
make install-git
echo "done" > dotfiles_fifo
sleep 5
tmux -L dotfiles kill-session
