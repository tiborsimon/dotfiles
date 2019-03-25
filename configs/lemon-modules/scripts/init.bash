#!/usr/bin/bash

systemctl --user import-environment PATH DISPLAY XAUTHORITY TERM &&
  systemctl --user restart lemon-modules.service &&
  systemctl --user restart lemon-modules-scheduler.timer &&
  lemon-modules-update --event startup
