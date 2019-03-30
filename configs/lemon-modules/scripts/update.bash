#!/usr/bin/bash

set -e

cd $(dirname $(readlink -f $0))

# ===================================================================
#  LOADING CONFIG

source ~/.config/lemon-modules/config.bash
source ./module-utils.bash

if [[ ! -p $NAMED_PIPE ]]; then
  echo "[ !! ][update] Lemon modules pipe ${NAMED_PIPE} not found! Aborting.."
  exit 1
fi


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
    warning "Invalid parameter: $key"
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
    >&2 echo -e "[ .. ][update] $@"
  fi
}


# ===================================================================
#  GENERATING MODULE POSITIONS

function sort_modules {
  local modules=$1
  local module_sorting_string=""

  for module in $modules
  do
    local priority=$(modules_get_priority $module)
    module_sorting_string="$module_sorting_string ${priority},${module}"
  done

  module_sorting_string=$(echo $module_sorting_string | tr " " "\n" | sort)

  for line in $module_sorting_string
  do
    local module=$(echo $line | cut -d',' -f2)
    echo $module
  done
}

function update_modules {
  local modules=$(get_module_list)
  local sorted_modules=$(sort_modules "$modules")

  for module in $sorted_modules
  do
    local update_events=$(modules_get_update_events $module)

    if echo "$update_events" | grep -q "${EVENT}" &>/dev/null
    then

      local render_script="${module}/${MODULE_RENDER_SCRIPT_NAME}"
      local cache_file="${module}/${MODULE_CACHE_FILE_NAME}"

      debug "Module [$(basename $module)] will be updated."
      local render_output="$($render_script $cache_file)"
      if [ -n "$render_output" ]
      then
        echo "$render_output" | sed -e 's/^/\[ \.\. \]\[update\]\[render\] /'
      fi
      debug "Module [$(basename $module)] rendered: $(cat $cache_file)"

      render "$modules"
    fi
  done
}

function get_position_string {
  local modules=$1

  for module in $modules
  do
    local position=$(modules_get_position $module)

    if [ ! -f ${module}/${MODULE_CACHE_FILE_NAME} ]
    then
      echo "" > ${module}/${MODULE_CACHE_FILE_NAME}
    fi

    local cached_content=$(cat ${module}/${MODULE_CACHE_FILE_NAME})
    echo "${position},${cached_content}"
  done
}

function render {
  local modules=$1

  local position_strings=$(get_position_string "$modules")

  local left_contents="$(get_modules_for_position "$position_strings" left)"
  local center_contents="$(get_modules_for_position "$position_strings" center)"
  local right_contents="$(get_modules_for_position "$position_strings" right)"

  IFS=$'\n'
  local left="%{l}"
  for content in $left_contents
  do
    left="${left} ${content} ${LEFT_SEPARATOR}"
  done

  local center="%{c}"
  for content in $center_contents
  do
    center="${center}${content}"
  done

  local right=""
  for content in $right_contents
  do
    right="${RIGHT_SEPARATOR} ${content} ${right}"
  done
  right="%{r}${right}"
  unset IFS

  echo -e "${left} ${center} ${right}" > ${NAMED_PIPE}
  debug "Updated content sent out to the named pipe."
}

function get_modules_for_position {
  local module_positions=$1
  local position=$2

  echo "$module_positions" |
    grep -P "^${position}" |
    sort |
    cut -d',' -f2
}

debug "Update request received for <${EVENT}> event."
update_modules
