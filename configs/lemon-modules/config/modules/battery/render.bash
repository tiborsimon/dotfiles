#!/usr/bin/env bash

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
      font="%{T1}"
      ;;
    'Discharging')
      icon_set=$(echo ${ICONS} | cut -d'.' -f 3)
      font="%{T4}"
      ;;
    'Charging')
      icon_set=$(echo ${ICONS} | cut -d'.' -f 2)
      font="%{T5}"
      ;;
  esac

  item=$(python -c "import math;c=math.floor(((${capacity}+4)/10)-1);print(c)")

  if [[ ${item} == -1 ]]; then
    echo "%{T1}${ICON_EMPTY}%{T1}"
  else
    echo "${font}${icon_set:${item}:1}%{T1}"
  fi
}

function render_ac {
  ac_present=$(cat /sys/class/power_supply/AC/online)

  if [[ ${ac_present} -eq 1 ]]; then
    echo "%{T1}${ICON_AC} "
  else
    echo ""
  fi
}

function get_current_percentage {
  local percentage=$(acpi | grep --ignore-case -P '(discharging|charging)' | grep --perl-regexp --only-matching '[\d]{1,3}%')
  if [ -n "${percentage}" ]
  then
    echo "${percentage} "
  else
    echo ""
  fi
}

ac=$(render_ac)
percentage=$(get_current_percentage)
battery_1=$(render_battery 1)
battery_2=$(render_battery 0)

echo -en "%{A:battery:}${ac}${percentage}${battery_1} ${battery_2}%{A}%{T1}"
