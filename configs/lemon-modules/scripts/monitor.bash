#!/usr/bin/env bash

function monitor_battery {
  udevadm monitor -p | grep --line-buffered 'POWER_SUPPLY_NAME=BAT.' |
  while read val; do
    lemon-modules-update --event battery
    echo "[ .. ][monitor] <battery> event fired."
    sleep 0.5
  done
}

function monitor_lid {
  acpi_listen |
  while read event; do
    if echo $event | grep -q "LID open"
    then
      lemon-modules-update --event lid_open
      echo "[ .. ][monitor] <lid_open> event fired."
    fi
  done
}

monitor_battery &
monitor_lid
