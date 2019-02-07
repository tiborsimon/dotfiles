#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

FONT1="SauceCodePro Nerd Font Mono:pixelsize=12"
FONT2="SauceCodePro Nerd Font Mono:pixelsize=15"
FONT3="SauceCodePro Nerd Font Mono:pixelsize=25"

./lemonbar-draw.bash | lemonbar -p \
                 -F\#FFFFFF -B\#1B1D1E \
                 -o -8 -f "${FONT1}" \
                 -o -6 -f "${FONT2}" \
                 -o 0 -f "${FONT3}" \
                 -g 1366x20 | ./lemonbar-command.bash
