#!/usr/bin/env bash

function monitor_battery {
  udevadm monitor -p | grep --line-buffered 'POWER_SUPPLY_NAME=BAT.' |
  while read val; do
    my-lemonbar-update --event battery
    echo "lemonbar-monitor - battery event"
    sleep 0.5
  done
}

function monitor_lid {
  acpi_listen |
  while read event; do
    if echo $event | grep -q "LID open"
    then
      my-lemonbar-update --event lid_open
      echo "lemonbar-monitor - lid open event"
    fi
  done
}

monitor_battery &
monitor_lid
