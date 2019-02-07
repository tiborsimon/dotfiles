#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_packages emacs adobe-source-code-pro-fonts
