#!/usr/bin/bash

set -e

cd $(dirname $(readlink -f $0))

# ===================================================================
#  LOADING CONFIG

source ~/.config/lemon-modules/config.bash
source ./module-utils.bash

if [[ ! -p $NAMED_PIPE ]]; then
  echo "[ !! ][update] Lemon modules pipe ${NAMED_PIPE} not found! Aborting.."
  # exit 1
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
    >&2 echo -e "[update][debug] $@"
  fi
}


# ===================================================================
#  GENERATING MODULE POSITIONS

function sort_modules {
  local module_config_files=$1
  local module_sorting_string=""

  debug "Sorting modules.."

  for module_config_file in $module_config_files
  do
    priority=$(modules_get_priority $module_config_file)
    module_sorting_string="$module_sorting_string ${priority},${module_config_file}"
    debug "${priority} -> ${module_config_file}"
  done

  module_sorting_string=$(echo $module_sorting_string | tr " " "\n" | sort)

  for line in $module_sorting_string
  do
    config=$(echo $line | cut -d',' -f2)
    debug "${config}"
    echo $config
  done
}

function update_module_caches {
  local module_config_files=$1
  local sorted_module_config_files=$(sort_modules "$module_config_files")

  for module_config_file in $sorted_module_config_files
  do
    update_events=$(modules_get_update_events $module_config_file)

    if echo $update_events | grep -q ${EVENT} &>/dev/null
    then
      module_path=$(dirname $module_config_file)
      rendered_content="$(${module_path}/${MODULE_RENDER_SCRIPT_NAME})"
      echo "$rendered_content" > ${module_path}/${MODULE_CACHE_FILE_NAME}
      debug "rendering new content.."
      render $module_config_files
    fi
  done
}

function get_position_string {
  local module_config_files=$1

  for module_config_file in $sorted_module_config_files
  do
    module_path=$(dirname $module_config_file)
    position=$(modules_get_position $module_config_file)

    if [ ! -f ${module_path}/${MODULE_CACHE_FILE_NAME} ]
    then
      echo "" > ${module_path}/${MODULE_CACHE_FILE_NAME}
    fi

    rendered_content=$(cat ${module_path}/${MODULE_CACHE_FILE_NAME})
    echo "${position},${rendered_content}"
  done
}

function render {
  local module_config_files=$1

  position_strings=$(get_position_string "$module_config_files")

  left_contents="$(get_modules_for_position "${position_strings}" left)"
  center_contents="$(get_modules_for_position "${position_strings}" center)"
  right_contents="$(get_modules_for_position "${position_strings}" right)"

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

  echo -e "${left} ${center} ${right}" > $NAMED_PIPE
  debug "${left} ${center} ${right}"

}

function get_modules_for_position {
  local module_positions=$1
  local position=$2

  echo "$module_positions" |
    grep -P "^${position}" |
    sort |
    cut -d',' -f2
}

module_config_files=$(modules_get_config_file_list)
update_module_caches "$module_config_files"
