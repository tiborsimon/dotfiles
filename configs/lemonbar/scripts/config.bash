#!/usr/bin/env bash

LEMONBAR_NAMED_PIPE="${HOME}/.lemonbar-pipe"

FG_COLOR="AFAFAF"
BG_COLOR="262626"

SCREEN_WIDTH="$(xdpyinfo | awk '/dimension/{print $2;}' | cut -dx -f1)"
BAR_HEIGHT="20"
DIMENSIONS="${SCREEN_WIDTH}x${BAR_HEIGHT}"

FONT_1_SPEC="SauceCodePro Nerd Font Mono:size=9"
FONT_2_SPEC="SauceCodePro Nerd Font Mono:size=10"
FONT_3_SPEC="SauceCodePro Nerd Font Mono:size=11"
FONT_4_SPEC="SauceCodePro Nerd Font Mono:size=12"
FONT_5_SPEC="SauceCodePro Nerd Font Mono:size=15"

FONT_1_OFFSET="-5"
FONT_2_OFFSET="-4"
FONT_3_OFFSET="-3"
FONT_4_OFFSET="-2"
FONT_5_OFFSET="0"

FONT_1="%{T1}"
FONT_2="%{T2}"
FONT_3="%{T3}"
FONT_4="%{T4}"
FONT_5="%{T5}"
