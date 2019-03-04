#!/usr/bin/bash

set -e

cd $(dirname $(readlink -f $0))

# ===================================================================
#  LOADING CONFIG

source ./config.bash

if [[ ! -p $LEMONBAR_NAMED_PIPE ]]; then
  echo "ERROR: Lemonbar pipe $LEMONBAR_NAMED_PIPE not found!"
  exit 1
fi


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
DEBUG=false
EVENT=undefined

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  -h|--help)
    HELP=true
    shift
    ;;
  -e|--event)
    EVENT=$2
    shift
    shift
    ;;
  --debug)
    DEBUG=true
    shift
    ;;
  *)
    warning "Invalid parameter: ${BOLD}${RED}$key${RESET}"
    shift
    ;;
esac
done


# ====================================================================
#  H E L P   C O M M A N D

if [ $HELP == true ]
then

  read -r -d '' help_message << EOM

${BOLD}DESCRIPTION${RESET}
    Generates the ${BOLD}lemonbar${RESET} content string by calling the ${BOLD}render script${RESET}
    of the installed plugins. It also ${BOLD}sorts${RESET} and ${BOLD}positions${RESET} the plugins
    in the lemonbar according to the plugins' ${BOLD}position file${RESET}.
    It uses a ${BOLD}named pipe${RESET} for sending the content to the lemonbar process.

${BOLD}USAGE${RESET}
    ${BOLD}${YELLOW}[-h|--help]${RESET}
        Prints out this help message.

    ${BOLD}${GREEN}(-e|--event) <event_name>${RESET}
        Event that happened to update the lemonbar. This can be used to cache certain modules until its
        event is triggering.

        Already registered event:

        ${BOLD}startup${RESET} - called at startup once
        ${BOLD}default${RESET} - when no event specified this event will be fired

    ${BOLD}${BLUE}--debug${RESET}
        Prints out the lemonbar content string instead of sending it
        to the lemonbar process.
EOM
  echo -e "\n$help_message"
  exit 0
fi

function debug {
  if [ $DEBUG == true ]
  then
    >&2 echo -e "debug >> $@"
  fi
}


# ===================================================================
#  GENERATING MODULE POSITIONS

MODULES_PATH="./modules"
MODULE_RENDER_SCRIPT="render.bash"
MODULE_POSITION_FILE="module_position"
MODULE_EVENTS_FILE="module_events"
MODULE_CACHE_FILE=".module_cache"

function generate_module_position_strings {
  for module_path in $1
  do
    debug "[$module_path]"
    if [ ${EVENT} == "startup" ] || grep -q "${EVENT}" ${module_path}/${MODULE_EVENTS_FILE}&>/dev/null
    then
      debug "module will be rendered!"
      content="$(${module_path}/${MODULE_RENDER_SCRIPT})"
      debug "module rendred. content: \"$content\""

      echo "$content" > ${module_path}/${MODULE_CACHE_FILE}
      debug "cache updated"

      if [ -z "$content" ]
      then
        debug "module won't be displayed as it's content is empty"
        continue
      fi
    else
      content="$(cat ${module_path}/${MODULE_CACHE_FILE})"
      debug "module rendered from cache"
    fi

    echo $(cat ${module_path}/${MODULE_POSITION_FILE})="${content}"
  done
}

function get_modules_for_position {
  local module_positions=$1
  local position=$2

  echo "$module_positions" |
    grep -P "^${position}" |
    sort |
    cut -d'=' -f2
}

modules=$(find ${MODULES_PATH} -mindepth 1 -type d)
position_strings=$(generate_module_position_strings "$modules")

left_contents="$(get_modules_for_position "${position_strings}" left)"
center_contents="$(get_modules_for_position "${position_strings}" center)"
right_contents="$(get_modules_for_position "${position_strings}" right)"


# ===================================================================
#  ASSEMBLING SECTIONS

LEFT_SEP=""
RIGHT_SEP=""

IFS=$'\n'
left="%{l}"
for content in $left_contents
do
  left="${left} ${content} ${LEFT_SEP}"
done

center="%{c}"
for content in $center_contents
do
  center="${center}${content}"
done

right=""
for content in $right_contents
do
  right="${RIGHT_SEP} ${content} ${right}"
done
right="%{r}${right}"
unset IFS


# ===================================================================
#  SENDING CONTENT TO LEMONBAR

echo -e "${left} ${center} ${right}" > $LEMONBAR_NAMED_PIPE
debug "${left} ${center} ${right}"
