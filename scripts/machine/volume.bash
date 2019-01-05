#!/usr/bin/env bash

#==============================================================================
#  GLOBAL VARIABLES

NAME="VOLUME"


#==============================================================================
#  H E L P E R S

BOLD=$(tput bold)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)

info () {
  printf "[ ${BOLD}${BLUE}>>${RESET} ] $1
"
}

success () {
  printf "[ ${BOLD}${GREEN}OK${RESET} ] $1
"
}

warning () {
  printf "[ ${BOLD}${YELLOW}!!${RESET} ] $1
"
}

error () {
  printf "[${BOLD}${RED}!!!!${RESET}] $1
"
}


#==============================================================================
#  P A R A M E T E R   P A R S I N G

HELP=true
VOLUME=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  h|help|-h|--help)
    shift
    ;;
  *)
    VOLUME=$key
    HELP=false
    shift
    ;;
esac
done

#==============================================================================
#  H E L P   C O M M A N D

if [ $HELP == true ]; then
  echo ""
  echo "  ${BOLD}${NAME}${RESET} usage"
  echo
  echo "  Set the system volume above 100%."
  echo
  echo "    [${BOLD}help${RESET}|${BOLD}h${RESET}]   - Prints out this help text."
  echo "    ${BOLD}volume${RESET}     - Desired volume without the % sign."
  echo ""
  exit 0
fi


#==============================================================================
#  P A R A M   C O M M A N D

if [ $HELP == false ]; then

  
  info "Setting system volume to ${VOLUME}%%.."
  pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo ${VOLUME}%

  exit 0
fi