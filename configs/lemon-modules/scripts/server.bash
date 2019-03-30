#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

source ~/.config/lemon-modules/config.bash

rm -f ${NAMED_PIPE}
mkfifo ${NAMED_PIPE}
trap "rm -f ${NAMED_PIPE}" EXIT

function monitor_battery {
  udevadm monitor -p | grep --line-buffered 'POWER_SUPPLY_NAME=BAT.' |
  while read val; do
    lemon-modules-update --event battery
    echo "[ .. ][monitor] <battery> event fired."
    sleep 0.5
  done
}

monitor_battery &

# open op the pipe in read write mode
cat <>${NAMED_PIPE} | lemonbar -p \
                 -F\#${FG_COLOR} -B\#${BG_COLOR} \
                 -o ${FONT_1_OFFSET} -f "${FONT_1_SPEC}" \
                 -o ${FONT_2_OFFSET} -f "${FONT_2_SPEC}" \
                 -o ${FONT_3_OFFSET} -f "${FONT_3_SPEC}" \
                 -o ${FONT_4_OFFSET} -f "${FONT_4_SPEC}" \
                 -o ${FONT_5_OFFSET} -f "${FONT_5_SPEC}" \
                 -g ${DIMENSIONS} | lemon-modules-click-handler
