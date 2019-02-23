#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# For volume management
install_packages pulseaudio pulseaudio-alsa pulsemixer alsa-utils
