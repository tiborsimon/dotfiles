#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash



execute_with_privilege systemctl enable tlp.service
execute_with_privilege systemctl enable tlp-sleep.service

execute_with_privilege systemctl mask systemd-rfkill.service
execute_with_privilege systemctl mask systemd-rfkill.socket
