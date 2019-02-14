#!/usr/bin/env bash

function monitor_battery {
  udevadm monitor -p | grep --line-buffered 'POWER_SUPPLY_NAME=BAT.' |
  while read val; do
    my-lemonbar-update
    sleep 0.5
  done
}

monitor_battery
