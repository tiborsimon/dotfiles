#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# execute_with_privilege sudo usermod -a -G video $USER
# write_to_messages "  machine - You should log out then log back in to adjust the volume."
