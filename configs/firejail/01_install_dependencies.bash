#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages firejail xdg-dbus-proxy

write_to_messages " firejail - You should manually run the ${BOLD}firecfg${RESET} command to add the shims!"
write_to_messages " firejail - You should manually edit the ${BOLD}browser-allow-drm${RESET} config in the ${BOLD}/etc/firejail/firejail.config${RESET} file!"
