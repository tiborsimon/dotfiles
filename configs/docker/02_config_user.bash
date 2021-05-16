#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

if groups | grep -qv docker
then
  execute_with_privilege sudo usermod -a -G docker $USER
  write_to_messages " docker - You should log out then log back in to be able to use docker."
fi
