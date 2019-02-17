#!/usr/bin/env bash


# ====================================================================
#  HELPERS

BOLD=$(tput bold)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)

function info {
  printf "[ ${BOLD}${BLUE}>>${RESET} ][${BOLD}management${RESET}] $1"
}

function success {
  printf "[ ${BOLD}${GREEN}OK${RESET} ][${BOLD}management${RESET}] $1"
}

function warning {
  printf "[ ${BOLD}${YELLOW}!!${RESET} ][${BOLD}management${RESET}] $1"
}

function error {
  printf "[${BOLD}${RED}!!!!${RESET}][${BOLD}management${RESET}] $1"
}


# ====================================================================
#  PARAMETER PARSING

HELP=true
UP=false
DOWN=false
CURRENT=false

match=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  h|help)
    match=true
    shift
    ;;
  u|up)
    UP=true
    match=true
    HELP=false
    shift
    ;;
  d|down)
    DOWN=true
    match=true
    HELP=false
    shift
    ;;
  current|c)
    CURRENT=true
    match=true
    HELP=false
    shift
    ;;
  *)
    warning "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    shift
    ;;
esac
done

if [ ${match} == false ]
then
  error "Missing argumnets. All parameters have to be provided!"
  HELP=true
fi

# ====================================================================
#  HELP COMMAND

if [ $HELP == true ]
then

  read -r -d '' help_message << EOM

${BOLD}DESCRIPTION${RESET}
    Quick interface for adjusting the volume of the machine.

${BOLD}USAGE${RESET}


    ${BOLD}${GREEN}h, help${RESET}
        Prints out this help message.

    ${BOLD}${GREEN}m, mute${RESET}
        Prints out this help message.
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

  echo $new_brightness > /sys/class/backlight/intel_backlight/brightness
}

if [ $UP == true ]
then
  change_brightness "+$(get_increment_for_percent 5)"
  exit 0
fi

if [ $DOWN == true ]
then
  change_brightness "-$(get_increment_for_percent 5)"
  exit 0
fi

if [ $CURRENT == true ]
then
  echo $(get_current_brightness_in_percentage)
  exit 0
fi
