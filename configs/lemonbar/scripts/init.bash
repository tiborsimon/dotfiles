#!/usr/bin/bash

systemctl --user import-environment PATH DISPLAY XAUTHORITY TERM
systemctl --user restart lemonbar.service
systemctl --user restart lemonbar-scheduler.timer
my-lemonbar-update
