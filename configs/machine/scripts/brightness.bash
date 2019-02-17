#!/usr/bin/env bash

# ====================================================================
#  HELPERS

BOLD=$(tput bold)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)


# ====================================================================
#  PARAMETER PARSING

HELP=true
UP=false
DOWN=false
SET=false
CURRENT=false
DEBUG=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  h|help)
    shift
    ;;
  u|up)
    UP=$2
    HELP=false
    shift
    shift
    ;;
  d|down)
    DOWN=$2
    HELP=false
    shift
    shift
    ;;
  s|set)
    SET=$2
    HELP=false
    shift
    shift
    ;;
  c|current|g|get)
    CURRENT=true
    HELP=false
    shift
    ;;
  --debug)
    DEBUG=true
    HELP=false
    shift
    ;;
  *)
    echo "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    shift
    ;;
esac
done

# ====================================================================
#  HELP COMMAND

if [ $HELP == true ]
then

  read -r -d '' help_message << EOM

${BOLD}DESCRIPTION${RESET}
    Quick interface to adjust the brightness of the screen.

${BOLD}USAGE${RESET}

    ${BOLD}${GREEN}[h|help]${RESET}
        Prints out this help message.

    ${BOLD}${GREEN}(u|up) <amount>${RESET}
        Increases the brightness by the given percentage amount.

    ${BOLD}${GREEN}(d|down) <amount>${RESET}
        Decreases the brightness by the given percentage amount.

    ${BOLD}${GREEN}(s|set) <amount>${RESET}
        Set the brightness to the given percentage.

    ${BOLD}${GREEN}(c|current|g|get)${RESET}
        Prints out the current screen brightness in percentage.
EOM
  echo "$help_message"
  exit 0
fi


# ====================================================================
#  BRIGHTNESS ADJUSTMENTS

function get_current_brightness_in_percentage {
  local max=$(cat /sys/class/backlight/intel_backlight/max_brightness)
  local current=$(cat /sys/class/backlight/intel_backlight/brightness)
  echo "scale=2; b=${current}/${max}; scale=0; b*100/1" | bc
}

function get_current_brightness {
  echo $(cat /sys/class/backlight/intel_backlight/brightness)
}

function get_increment_for_percent {
  local max=$(cat /sys/class/backlight/intel_backlight/max_brightness)
  local increment=$1

  if (( ${increment} > 100 ))
  then
    increment=100
  fi

  if (( ${increment} < 1 ))
  then
    increment=1
  fi

  echo "scale=2; i=${max}/100; scale=0; i*${increment}/1" | bc
}

function change_brightness {
  local max=$(cat /sys/class/backlight/intel_backlight/max_brightness)
  local increment=$1
  local current=$(get_current_brightness)
  local new_brightness=$(echo "${current}${increment}" | bc)

  if (( ${new_brightness} > ${max} ))
  then
    new_brightness=${max}
  fi

  if (( ${new_brightness} < 1 ))
  then
    new_brightness=1
  fi

  set_brightness $new_brightness
}

function set_brightness {
  brightness=$1
  if [ $DEBUG == true ]
  then
    echo $brightness
  else
    echo $brightness > /sys/class/backlight/intel_backlight/brightness
  fi
}

if [ $UP != false ]
then
  increment=$(get_increment_for_percent ${UP})
  change_brightness "+${increment}"
  exit 0
fi

if [ $DOWN != false ]
then
  increment=$(get_increment_for_percent ${DOWN})
  change_brightness "-${increment}"
  exit 0
fi

if [ $SET != false ]
then
  increment=$(get_increment_for_percent ${SET})
  set_brightness $increment
  exit 0
fi

if [ $CURRENT == true ]
then
  echo $(get_current_brightness_in_percentage)
  exit 0
fi
