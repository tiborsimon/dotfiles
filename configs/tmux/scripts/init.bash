#!/usr/bin/env bash
# Tmux starter script.

tmux new-session -d -s tibor
tmux new-window -c ~
tmux new-window -c ~
tmux new-window -c ~
tmux new-window -c ~
tmux new-window -c ~
tmux new-window -c ~
tmux new-window -c ~
tmux new-window -c ~
tmux new-window -c ~
tmux select-window -t 1

tmux -2 attach-session -t tibor
