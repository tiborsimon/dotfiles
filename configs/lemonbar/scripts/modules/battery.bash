#!/usr/bin/env bash

source $1

ICONS="..ﴆﴇﴈﴉﴊﴋﴌﴍﴎﴅ"
ICON_EMPTY=""
ICON_UNKNOWN=""
ICON_FULLY_CHARGED=""
ICON_AC="ﮣ"

function render_battery {
  index=$1
  state=$(cat /sys/class/power_supply/BAT${index}/status)
  capacity=$(cat /sys/class/power_supply/BAT${index}/capacity)
  case $state in
    'Full')
      echo ${ICON_FULLY_CHARGED}
      return 0
      ;;
    'Unknown')
      if (( ${capacity} <= 5 )); then
        echo ${ICON_EMPTY}
        return 0
      fi
      icon_set=$(echo ${ICONS} | cut -d'.' -f 1)
      font="${FONT_1}"
      ;;
    'Discharging')
      icon_set=$(echo ${ICONS} | cut -d'.' -f 3)
      font="${FONT_4}"
      ;;
    'Charging')
      icon_set=$(echo ${ICONS} | cut -d'.' -f 2)
      font="${FONT_5}"
      ;;
  esac

  item=$(python -c "import math;c=math.floor(((${capacity}+4)/10)-1);print(c)")

  if [[ ${item} == -1 ]]; then
    echo "${FONT_1}${ICON_EMPTY}${FONT_1}"
  else
    echo "${font}${icon_set:${item}:1}${FONT_1}"
  fi
}

function render_ac {
  ac_present=$(cat /sys/class/power_supply/AC/online)

  if [[ ${ac_present} -eq 1 ]]; then
    echo "${FONT_1}${ICON_AC} "
  else
    echo ""
  fi
}

AC=$(render_ac)
BATTERY_1=$(render_battery 1)
BATTERY_2=$(render_battery 0)

echo -en "%{A:battery:}${AC}${BATTERY_1} ${BATTERY_2}%{A}${FONT_1}"
