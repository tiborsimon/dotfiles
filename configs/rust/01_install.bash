#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_packages rustup rust

install_packages cmake

cargo intsall exa
intsall_packages hexyl fd bat

