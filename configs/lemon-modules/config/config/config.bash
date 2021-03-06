#!/usr/bin/env bash

NAMED_PIPE="${HOME}/.cache/lemon-modules.pipe"

CONFIG_PATH="${HOME}/.config/lemon-modules"
MODULES_PATH="${CONFIG_PATH}/modules"
MODULE_POSITION_FILE_NAME="position.conf"
MODULE_PRIORITY_FILE_NAME="priority.conf"
MODULE_SCHEDULE_FILE_NAME="schedule.conf"
MODULE_UPDATE_ON_FILE_NAME="update_on.conf"
MODULE_CACHE_FILE_NAME=".module.cache"
MODULE_RENDER_SCRIPT_NAME="render.bash"

FG_COLOR="AFAFAF"
BG_COLOR="262626"

LEFT_SEPARATOR=""
RIGHT_SEPARATOR=""

SCREEN_WIDTH="$(xdpyinfo | awk '/dimension/{print $2;}' | cut -dx -f1)"
BAR_HEIGHT="18"
DIMENSIONS="${SCREEN_WIDTH}x${BAR_HEIGHT}"

FONT_1_SPEC="SauceCodePro Nerd Font Mono:size=9"
FONT_2_SPEC="SauceCodePro Nerd Font Mono:size=11"
FONT_3_SPEC="SauceCodePro Nerd Font Mono:size=12"
FONT_4_SPEC="SauceCodePro Nerd Font Mono:size=16"
FONT_5_SPEC="SauceCodePro Nerd Font Mono:size=15"

FONT_1_OFFSET="-5"
FONT_2_OFFSET="-4"
FONT_3_OFFSET="-2"
FONT_4_OFFSET="1"
FONT_5_OFFSET="0"
