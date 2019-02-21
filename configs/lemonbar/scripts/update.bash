#!/usr/bin/bash
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

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  -h|--help)
    HELP=true
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

    ${BOLD}${YELLOW}--debug${RESET}
        Prints out the lemonbar content string instead of sending it
        to the lemonbar process.
EOM
  echo -e "\n$help_message"
  exit 0
fi


# ===================================================================
#  GENERATING MODULE POSITIONS

MODULES_PATH="./modules"
MODULE_RENDER_SCRIPT="render.bash"

function generate_module_position_strings {
  for module in $1
  do
    echo "$(cat $module/position)=${module}/${MODULE_RENDER_SCRIPT}"
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

left_modules=$(get_modules_for_position "${position_strings}" left)
center_modules=$(get_modules_for_position "${position_strings}" center)
right_modules=$(get_modules_for_position "${position_strings}" right)


# ===================================================================
#  ASSEMBLING SECTIONS

LEFT_SEP=""
RIGHT_SEP=""

left="%{l}"
for render_module in $left_modules
do
  output=$(${render_module})
  if [ -n "$output" ]
  then
    left="${left} ${output} ${LEFT_SEP}"
  fi
done
# left="${left::-1}"

center="%{c}"
for render_module in $center_modules
do
  output=$(${render_module})
  if [ -n "$output" ]
  then
    center="${center}$(${render_module})"
  fi
done

right=""
for render_module in $right_modules
do
  output=$(${render_module})
  if [ -n "$output" ]
  then
    right="${RIGHT_SEP} $(${render_module}) ${right}"
  fi
done
# right="%{r}${right:1:${#right}-1}"
right="%{r}${right}"


# ===================================================================
#  SENDING CONTENT TO LEMONBAR

if [ $DEBUG == true ]
then
  echo -e "${left} ${center} ${right}"
else
  echo -e "${left} ${center} ${right}" > $LEMONBAR_NAMED_PIPE
fi
