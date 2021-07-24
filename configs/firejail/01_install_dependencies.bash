#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages firejail xdg-dbus-proxy

write_to_messages " firejail - You should manually run the firecfg command to add the shims!"
