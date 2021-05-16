#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

TARGET_DIR='/etc/systemd/logind.conf.d'

if [ ! -d "$TARGET_DIR" ]
then
  execute_with_privilege mkdir -pv "$TARGET_DIR"
fi

execute_with_privilege cp -v ./config/00-logind.custom.conf "${TARGET_DIR}/00-logind.custom.conf"
