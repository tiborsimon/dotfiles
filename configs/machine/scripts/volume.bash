#!/usr/bin/env bash

if gpreg my-machine-volume &>/dev/null
then
  exit 0
fi

VOLUME_CACHE_FILE="${HOME}/.cache/dotfiles/machine/volume/volume.cache"
MUTE_CACHE_FILE="${HOME}/.cache/dotfiles/machine/volume/mute.cache"

# ====================================================================
#  H E L P E R S

# ====================================================================
#  P A R A M E T E R   P A R S I N G

HELP=true
MUTE=false
UNMUTE=false
TOGGLE=false
VOLUME_UP=false
VOLUME_DOWN=false
VOLUME=false
IS_MUTED=false
DEBUG=false
UPDATE_LEMONBAR=true

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  -h|--help)
    shift
    ;;
  m|mute)
    MUTE=true
    HELP=false
    shift
    ;;
  unmute)
    UNMUTE=true
    HELP=false
    shift
    ;;
  t|toggle)
    TOGGLE=true
    HELP=false
    shift
    ;;
  u|up)
    VOLUME_UP=$2
    HELP=false
    shift
    shift
    ;;
  d|down)
    VOLUME_DOWN=$2
    HELP=false
    shift
    shift
    ;;
  c|current)
    VOLUME=true
    HELP=false
    shift
    ;;
  muted|is_muted)
    IS_MUTED=true
    HELP=false
    shift
    ;;
  --debug)
    DEBUG=true
    shift
    ;;
  --no-lemonbar)
    UPDATE_LEMONBAR=false
    shift
    ;;
  *)
    echo "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    shift
    ;;
esac
done


# ====================================================================
#  H E L P   C O M M A N D

if [ $HELP == true ]
then

  BOLD=$(tput bold)
  RED=$(tput setaf 1)
  GREEN=$(tput setaf 2)
  YELLOW=$(tput setaf 3)
  BLUE=$(tput setaf 4)
  RESET=$(tput sgr0)

  read -r -d '' help_message << EOM

${BOLD}DESCRIPTION${RESET}
    Quick interface to adjust the master volume of the machine.

${BOLD}USAGE${RESET}
    ${BOLD}${BLUE}[-h|--help]${RESET}
        Prints out this help message.

    ${BOLD}${BLUE}[--debug]${RESET}
        Prints out the new volume value instead of applying it.

    ${BOLD}${YELLOW}(m|mute)${RESET}
        Mutes the system master volume.

    ${BOLD}${YELLOW}unmute${RESET}
        Unmutes the system master volume.

    ${BOLD}${YELLOW}(t|toggle)${RESET}
        Toggles the system master volume mute state.

    ${BOLD}${YELLOW}(u|up) <amount>${RESET}
        Increases the system master volume by the given percentage amount.

    ${BOLD}${YELLOW}(d|down) <amount>${RESET}
        Decreases the system master volume by the given percentage amount.

    ${BOLD}${YELLOW}(c|current)${RESET}
        Prints out the current system master volume in percentage, then exits.

    ${BOLD}${YELLOW}(muted|is_muted)${RESET}
        Returns 0 if the system master volume is muted, otherwise returns 1.
EOM
  echo -e "\n$help_message"
  exit 0
fi


# ====================================================================
#  H E L P E R   F U N C T I O N S

function debug {
  if [ $DEBUG == true ]
  then
    >&2 echo "update >> $@"
  fi
}

function get_current_volume {
  amixer get Master |
    grep -Po '[\d]{1,3}%' |
    sed -n '1p' |
    grep -Po '\d+' |
    cat
}

function change_volume {
  amount=$1

  current_volume=$(get_current_volume)

  new_volume=$(echo "${current_volume} ${amount}" | bc)

  if (( ${new_volume} > 100 ))
  then
    new_volume=100
  fi

  if (( ${new_volume} < 0 ))
  then
    new_volume=0
  fi

  if [ $DEBUG == true ]
  then
    echo ${new_volume}
  else
    amixer --quiet set Master ${new_volume}%
  fi
}

function mute_toggle {
  debug "toggling mute state"
  amixer --quiet set Master toggle
}

function mute {
  debug "muting autio.."
  amixer --quiet set Master mute
}

function unmute {
  debug "unmuting autio.."
  amixer --quiet set Master unmute
}


# ====================================================================
#  M U T E   C O M M A N D S

if [ $TOGGLE == true ]
then
  mute_toggle
  exit 0
fi

if [ $MUTE == true ]
then
  mute
  exit 0
fi

if [ $UNMUTE == true ]
then
  unmute
  exit 0
fi

if [ $IS_MUTED == true ]
then
  if amixer get Master | grep -q '\[off\]'
  then
    exit 0
  else
    exit 1
  fi
fi


# ====================================================================
#  V O L U M E   A D J U S T I N G   C O M M A N D S


if [ $VOLUME_UP != false ]
then
  change_volume "+${VOLUME_UP}"
  exit 0
fi

if [ $VOLUME_DOWN != false ]
then
  change_volume "-${VOLUME_DOWN}"
  exit 0
fi

if [ $VOLUME == true ]
then
  echo $(get_current_volume)
  exit 0
fi
