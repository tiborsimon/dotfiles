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
    -h|--help)
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

function debug {
  if [ $DEBUG == true ]
  then
    (>&2 echo $@)
  fi
}


# ====================================================================
#  HELP COMMAND

if [ $HELP == true ]
then

  read -r -d '' help_message << EOM

${BOLD}DESCRIPTION${RESET}
    Quick interface to adjust the brightness of the screen.

${BOLD}USAGE${RESET}

    ${BOLD}${BLUE}[-h|--help]${RESET}
        Prints out this help message.

    ${BOLD}${BLUE}[--debug]${RESET}
        Prints out the new brightness value instead of applying it.

    ${BOLD}${YELLOW}(u|up) <amount>${RESET}
        Increases the brightness by the given percentage amount.

    ${BOLD}${YELLOW}(d|down) <amount>${RESET}
        Decreases the brightness by the given percentage amount.

    ${BOLD}${YELLOW}(s|set) <amount>${RESET}
        Set the brightness to the given percentage.

    ${BOLD}${YELLOW}(c|current|g|get)${RESET}
        Prints out the current screen brightness in percentage.
EOM
  echo -e "\n$help_message"
  exit 0
fi


# ====================================================================
#  BRIGHTNESS ADJUSTMENTS

function get_current_brightness {
  local max=$(cat /sys/class/backlight/intel_backlight/max_brightness)
  local current=$(cat /sys/class/backlight/intel_backlight/brightness)
  echo $(python -c "print(round(${current}/${max}*100))")
}

function get_value_for_percentage {
  local target=$1
  local max=$(cat /sys/class/backlight/intel_backlight/max_brightness)

  debug "get_value_for_percentage <${target}> called"

  if (( ${target} > 100 ))
  then
    target=100
  fi

  if (( ${target} < 0 ))
  then
    target=0
  fi

  result=$(python -c "print(round(${target}/100*${max}))")
  debug "get_value_for_percentage -> <${result}>"
  echo $result
}

function set_brightness {
  local target=$1
  local brightness=$(get_value_for_percentage ${target})
  if [ $DEBUG == true ]
  then
    debug "final value: ${brightness}"
    echo $brightness
  else
    echo $brightness > /sys/class/backlight/intel_backlight/brightness
  fi
}

if [ $UP != false ]
then
  increment=${UP}
  current=$(get_current_brightness)
  target=$(python -c "print(${current} + ${increment})")
  set_brightness $target
  exit 0
fi

if [ $DOWN != false ]
then
  increment=${DOWN}
  current=$(get_current_brightness)
  target=$(python -c "print(${current} - ${increment})")
  set_brightness $target
  exit 0
fi

if [ $SET != false ]
then
  target=${SET}
  set_brightness $target
  exit 0
fi

if [ $CURRENT == true ]
then
  echo $(get_current_brightness)
  exit 0
fi
