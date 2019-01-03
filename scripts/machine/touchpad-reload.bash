#!/usr/bin/env bash

echo "Reloading touchpad driver.."
modprobe -r psmouse
modprobe psmouse

