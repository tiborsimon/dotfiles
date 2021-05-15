#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

execute_with_privilege mkdir -pv /etc/systemd/logind.conf.d
execute_with_privilege cp -v ./config/logind.custom.conf /etc/systemd/logind.conf.d/logind.custom.conf
