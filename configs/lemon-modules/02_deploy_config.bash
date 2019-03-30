#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

# linking config directory
link_package \
  ./config/config ${HOME}/.config/lemon-modules

execute_with_privilege rm -rf /etc/acpi
execute_with_privilege cp -r ./config/acpi /etc/acpi

# linking systemd unit files
link_package \
  ./config/lemon-modules.service           ${HOME}/.config/systemd/user/lemon-modules.service \
  ./config/lemon-modules-scheduler.service ${HOME}/.config/systemd/user/lemon-modules-scheduler.service \
  ./config/lemon-modules-scheduler.timer   ${HOME}/.config/systemd/user/lemon-modules-scheduler.timer

# linking command scripts
link_package \
  ./scripts/server.bash        ${HOME}/.local/bin/lemon-modules-server \
  ./scripts/update.bash        ${HOME}/.local/bin/lemon-modules-update \
  ./scripts/init.bash          ${HOME}/.local/bin/lemon-modules-init \
  ./scripts/click-handler.bash ${HOME}/.local/bin/lemon-modules-click-handler \
  ./scripts/scheduler.bash     ${HOME}/.local/bin/lemon-modules-scheduler
