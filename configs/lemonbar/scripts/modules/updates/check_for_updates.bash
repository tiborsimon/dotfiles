#!/usr/bin/env bash

echo "checking for updates.."

updates=$(my-machine-pacman check --no-lemonbar --count-only)
if (("$updates" > "0"))
then
  echo "$updates updates were found."
else
  echo "no updates were found."
fi

echo "checking for updates finished"

my-lemonbar-update --event update
