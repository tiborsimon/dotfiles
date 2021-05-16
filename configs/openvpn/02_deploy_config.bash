#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

# Based on this description:
# https://wiki.archlinux.org/title/OpenVPN#The_update-systemd-resolved_custom_script

# There is one difference though, the openvpn configs has to contain the following additional parts, as it is displayed in the post installation steps during the aur install:
# script-security 2
# up /usr/bin/update-systemd-resolved
# up-restart
# down /usr/bin/update-systemd-resolved
# down-pre

execute_with_privilege cp -fv ./config/00-openvpn-resolved.rules /etc/polkit-1/rules.d/00-openvpn-resolved.rules
