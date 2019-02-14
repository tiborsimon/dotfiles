#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

execute_with_privilege sudo usermod -a -G docker $USER
info "You should log out and log back in to be able to use docker"
