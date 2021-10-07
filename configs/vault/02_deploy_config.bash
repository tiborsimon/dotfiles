#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_scripts vault \
  './scripts/open.sh' \
  './scripts/suspend.sh' \
  './scripts/resume.sh' \
  './scripts/status.sh' \
  './scripts/close.sh'
