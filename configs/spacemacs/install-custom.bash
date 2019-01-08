#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
