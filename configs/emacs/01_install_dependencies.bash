#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# gcc needs to be installed here to be able to run emacs on manjaro.. Go figure..
install_packages emacs gcc
