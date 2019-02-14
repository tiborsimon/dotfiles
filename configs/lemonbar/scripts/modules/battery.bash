#!/usr/bin/env bash

source $1

ICONS="${FONT_1}\uf579${FONT_1}.${FONT_5}\uf585${FONT_1}|${FONT_1}\uf57a${FONT_1}.${FONT_5}\uf585${FONT_1}|${FONT_1}\uf57b${FONT_1}.${FONT_5}\uf586${FONT_1}|${FONT_1}\uf57c${FONT_1}.${FONT_5}\uf587${FONT_1}|${FONT_1}\uf57d${FONT_1}.${FONT_5}\uf588${FONT_1}|${FONT_1}\uf57e${FONT_1}.${FONT_5}\uf588${FONT_1}|${FONT_1}\uf57f${FONT_1}.${FONT_5}\uf589${FONT_1}|${FONT_1}\uf580${FONT_1}.${FONT_5}\uf589${FONT_1}|${FONT_1}\uf581${FONT_1}.${FONT_5}\uf58a${FONT_1}|${FONT_1}\uf578${FONT_1}.${FONT_5}\uf584${FONT_1}"

ICON_EMPTY="${FONT_1}\uf58d${FONT_1}"
ICON_UNKNOWN="${FONT_1}\uf590${FONT_1}"
ICON_FULLY_CHARGED="${FONT_1}\uf58e${FONT_1}"

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
      else
        echo ${ICON_UNKNOWN}
      fi
      return 0
      ;;
    'Discharging')
      type=1;;
    'Charging')
      type=2;;
  esac

  item=$(echo "${capacity} / 10" | bc)
  if (( ${item} == 0 )); then
    item=1
  fi
  echo $(echo ${ICONS} | cut -d'|' -f ${item} | cut -d'.' -f ${type} )
}

BATTERY_1=$(render_battery 1)
BATTERY_2=$(render_battery 0)

echo -en "%{A:battery:}${BATTERY_1} ${BATTERY_2}%{A}${FONT_1}"
