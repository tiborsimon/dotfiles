#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

# ===================================================================
#  LOADING CONFIG

source ./config.bash

if [[ ! -p $LEMONBAR_NAMED_PIPE ]]; then
  echo "ERROR: Lemonbar pipe $LEMONBAR_NAMED_PIPE not found!"
  exit 1
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
  left="${left} $(${render_module}) ${LEFT_SEP}"
done
left="${left::-1}"

center="%{c}"
for render_module in $center_modules
do
  center="${center}$(${render_module})"
done

right=""
for render_module in $right_modules
do
  right="${RIGHT_SEP} $(${render_module}) ${right}"
done
right="%{r}${right:1:${#right}-1}"


# ===================================================================
#  SENDING CONTENT TO LEMONBAR

# echo -e "${left} ${center} ${right}" > $LEMONBAR_NAMED_PIPE
echo -e "${left} ${center} ${right}"
