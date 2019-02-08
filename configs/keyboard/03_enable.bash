#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

SERVICE_NAME='udevmon.service'

info "Making sure udevmon service is enabled.."
if ! systemctl is-enabled $SERVICE_NAME &>/dev/null; then
  execute_with_privilege systemctl enable $SERVICE_NAME
  execute_with_privilege systemctl start $SERVICE_NAME
fi
