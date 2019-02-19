#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

# loading config with the predefined fonts
source ./config.bash

rm -f $LEMONBAR_NAMED_PIPE
mkfifo $LEMONBAR_NAMED_PIPE
trap "rm -f $LEMONBAR_NAMED_PIPE" EXIT

my-lemonbar-monitor &

# open op the pipe in read write mode
cat <>$LEMONBAR_NAMED_PIPE | lemonbar -p \
                 -F\#${FG_COLOR} -B\#${BG_COLOR} \
                 -o ${FONT_1_OFFSET} -f "${FONT_1_SPEC}" \
                 -o ${FONT_2_OFFSET} -f "${FONT_2_SPEC}" \
                 -o ${FONT_3_OFFSET} -f "${FONT_3_SPEC}" \
                 -o ${FONT_4_OFFSET} -f "${FONT_4_SPEC}" \
                 -o ${FONT_5_OFFSET} -f "${FONT_5_SPEC}" \
                 -g ${DIMENSIONS} | ./click-handler.bash
