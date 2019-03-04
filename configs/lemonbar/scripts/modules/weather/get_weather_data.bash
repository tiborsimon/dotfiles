#!/usr/bin/env bash

echo "started to collect weather report.."
# weather=$(curl wttr.in?format="+%t")
sleep 5
echo "weather report updated"

my-lemonbar-update --event weather
