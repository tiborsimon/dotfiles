#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package keyboard \
             ./config/udevmon.yaml /etc/udevmon.yaml
