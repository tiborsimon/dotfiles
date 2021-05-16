#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

execute_with_privilege cp -vf ./config/my-sudoers /etc/sudoers.d/my-sudoers
