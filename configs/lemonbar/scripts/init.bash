#!/usr/bin/bash

systemctl --user import-environment PATH DISPLAY XAUTHORITY
systemctl --user restart lemonbar.service
systemctl --user restart lemonbar-clock.timer
my-lemonbar-update
