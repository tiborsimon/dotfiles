#!/usr/bin/env bash

test "$(cat /sys/class/power_supply/AC/online)" -eq '1'
