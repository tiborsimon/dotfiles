#!/usr/bin/env bash

max=$(cat /sys/class/backlight/intel_backlight/max_brightness)

new_percent=$1
new=$(expr $max \* $new_percent / 100)
echo $new > /sys/class/backlight/intel_backlight/brightness
