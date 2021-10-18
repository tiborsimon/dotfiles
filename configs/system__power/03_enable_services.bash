#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

ensure_service() {
  target="$1"

  # Enable the given target.
  if [ "$(systemctl is-enabled ${target})" != 'enabled' ]
  then
    execute_with_privilege sudo systemctl enable "${target}"
  fi

  # Start the given target.
  if [ "$(systemctl is-active ${target})" != 'active' ]
  then
    execute_with_privilege sudo systemctl start "${target}"
  fi

  # Get the status of the given target.
  execute_with_privilege sudo systemctl status "${target}"
}

ensure_service 'tlp.service'

execute_with_privilege systemctl mask systemd-rfkill.service
execute_with_privilege systemctl mask systemd-rfkill.socket
