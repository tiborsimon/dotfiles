#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

install_packages openvpn
install_aur_packages openvpn-update-systemd-resolved
