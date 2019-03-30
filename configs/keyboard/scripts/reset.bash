#!/usr/bin/env bash

echo "Setting up repeate speed.."
if [ -n "$DISPLAY" ]
then
  xset r rate 270 55
fi

if systemctl is-failed udevmon.service&>/dev/null
then
  echo "Restarting udevmon service.."
  sudo systemctl restart udevmon.service
fi

echo "Done."

