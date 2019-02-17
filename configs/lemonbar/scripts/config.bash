#!/usr/bin/env bash

LEMONBAR_NAMED_PIPE="${HOME}/.lemonbar-pipe"

FG_COLOR="AFAFAF"
BG_COLOR="262626"

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
