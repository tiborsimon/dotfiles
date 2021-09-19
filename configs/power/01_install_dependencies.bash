#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages powertop tlp acpi_call-lts ethtool smartmontools

write_to_messages " power - You should run the ${BOLD}powertop --calibrate${RESET} and ${BOLD}powertop --auto-tune${RESET} commands."
