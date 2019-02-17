#!/usr/bin/env bash

# ====================================================================
#  H E L P E R S

BOLD=$(tput bold)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)


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

match=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  h|help)
    match=true
    shift
    ;;
  m|mute)
    MUTE=true
    match=true
    HELP=false
    shift
    ;;
  u|unmute)
    UNMUTE=true
    match=true
    HELP=false
    shift
    ;;
  t|toggle)
    TOGGLE=true
    match=true
    HELP=false
    shift
    ;;
  u|up)
    VOLUME_UP=$2
    match=true
    HELP=false
    shift
    shift
    ;;
  d|down)
    VOLUME_DOWN=$2
    match=true
    HELP=false
    shift
    shift
    ;;
  c|current)
    VOLUME=true
    match=true
    HELP=false
    shift
    ;;
  muted|is_muted)
    IS_MUTED=true
    match=true
    HELP=false
    shift
    ;;
  *)
    echo "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    shift
    ;;
esac
done

if [ ${match} == false ]
then
  HELP=true
fi


# ====================================================================
#  H E L P   C O M M A N D

if [ $HELP == true ]
then

  read -r -d '' help_message << EOM

${BOLD}DESCRIPTION${RESET}
    Quick interface to adjust the master volume of the machine.

${BOLD}USAGE${RESET}
    ${BOLD}${GREEN}[h|help]${RESET}
        Prints out this help message.

    ${BOLD}${GREEN}(m|mute)${RESET}
        Mutes the system master volume.

    ${BOLD}${GREEN}(u,unmute)${RESET}
        Unmutes the system master volume.

    ${BOLD}${GREEN}(t|toggle)${RESET}
        Toggles the system master volume mute state.

    ${BOLD}${GREEN}(u|up) <amount>${RESET}
        Increases the system master volume by the given percentage amount.

    ${BOLD}${GREEN}(d|down) <amount>${RESET}
        Decreases the system master volume by the given percentage amount.

    ${BOLD}${GREEN}(c|current)${RESET}
        Prints out the current system master volume in percentage, then exits.

    ${BOLD}${GREEN}(muted|is_muted)${RESET}
        Returns 0 if the system master volume is muted, otherwise returns 1.
EOM
  echo -e "\n$help_message"
  exit 0
fi


# ====================================================================
#  M U T E   C O M M A N D S

if [ $TOGGLE == true ]
then
  amixer --quiet set Master toggle
  exit 0
fi

if [ $MUTE == true ]
then
  amixer --quiet set Master mute
  exit 0
fi

if [ $UNMUTE == true ]
then
  amixer --quiet set Master unmute
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

  amixer --quiet set Master ${new_volume}%
}

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
