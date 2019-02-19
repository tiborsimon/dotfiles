#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

execute_with_privilege cp ./config/my-sudoers /etc/sudoers.d/my-sudoers
