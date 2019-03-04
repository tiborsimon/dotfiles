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

HELP=false
CHECK=true
UPGRADE=false
COUNT_ONLY=false
UPDATE_LEMONBAR=true
QUERY_ONLY=false
DEBUG=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  -h|--help)
    HELP=true
    shift
    ;;
  check)
    CHECK=true
    HELP=false
    shift
    ;;
  upgrade)
    UPGRADE=true
    CHECK=false
    shift
    ;;
  --debug)
    DEBUG=true
    shift
    ;;
  --count-only)
    COUNT_ONLY=true
    shift
    ;;
  --query-only)
    QUERY_ONLY=true
    shift
    ;;
  --no-lemonbar)
    UPDATE_LEMONBAR=false
    shift
    ;;
  *)
    echo "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    HELP=true
    break
    ;;
esac
done



# ====================================================================
#  H E L P   C O M M A N D

if [ $HELP == true ]
then

  read -r -d '' help_message << EOM

${BOLD}DESCRIPTION${RESET}
    PACMAN management tool with support for my custom lemonbar system.

${BOLD}USAGE${RESET}
    ${BOLD}${BLUE}[-h|--help]${RESET}
        Prints out this help message.

    ${BOLD}${YELLOW}[check]${RESET}
        Checks for updates and prints out the outdated packages. This is the
        default command.

    ${BOLD}${YELLOW}upgrade${RESET}
        Performs a system upgrade.

    ${BOLD}${GREEN}[--debug]${RESET}
        Prints debug messages to the standard error output during execution.

    ${BOLD}${GREEN}[--no-lemonbar]${RESET}
        Does not update lemonbar.

    ${BOLD}${GREEN}[--count-only]${RESET}
        Only prints out the number of updates.

    ${BOLD}${GREEN}[--query-only]${RESET}
        Only performs a local check based on the local database. This flag can
        be useful if we already know that there are updates available, and want
        to get the number of updates quickly.

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

function update_lemonbar {
  if [ $UPDATE_LEMONBAR == true ] && which my-lemonbar-update&>/dev/null
  then
    debug "updating lemonbar.."
    if my-lemonbar-update --event updates &>/dev/null
    then
      debug "lemonbar updated."
    else
      debug "error during lemonbar update.."
    fi
  fi
}

function check_for_updates {
  debug "checking and downloading updates.."
  sudo pacman --sync --refresh --refresh --sysupgrade --downloadonly --noconfirm &>/dev/null
}

function perform_system_upgrade {
  debug "updating system with pacman.."
  sudo pacman --sync --refresh --refresh --sysupgrade
}

function list_updates {
  debug "listing updates.."
  pacman --color always -Qu
}

function count_updates {
  debug "counting updates.."
  updates=$(pacman -Qu | wc -l)
  debug "$updates updates were found"
  echo $updates
}


# ====================================================================
#  C H E C K   C O M M A N D


if [ $CHECK == true ]
then
  debug "check command called"

  if [ $QUERY_ONLY == false ]
  then
    check_for_updates
  fi

  if [ $COUNT_ONLY == true ]
  then
    count_updates
  else
    list_updates
  fi

  update_lemonbar

  exit 0
fi


# ====================================================================
#  U P G R A D E   C O M M A N D

if [ $UPGRADE == true ]
then
  debug "upgrade command called"

  perform_system_upgrade
  updates=$(count_updates)
  update_lemonbar

  exit 0
fi
