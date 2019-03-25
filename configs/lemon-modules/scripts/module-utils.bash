#!/usr/bin/bash

if [ -z ${MODULES_PATH+x} ] || [ -z ${MODULE_CONFIG_FILE_NAME+x} ]
then
  echo "[ !! ][module-utils] Source the config file before the module-utils file!"
  exit 1
fi

function modules_get_config_file_list {
  find $MODULES_PATH -type f -name $MODULE_CONFIG_FILE_NAME
}

function filter_config_by_keyword {
  local config=$1
  local keyword=$2
  awk -v keyword="$keyword" '$0 ~ keyword {for (i=2; i<NF; i++) printf $i " "; print $NF}' $config
}

function modules_get_position {
  local config=$1
  echo $(filter_config_by_keyword $config POSITION)
}

function modules_get_priority {
  local config=$1
  echo $(filter_config_by_keyword $config PRIORITY)
}

function modules_get_schedule_pattern {
  local config=$1
  echo $(filter_config_by_keyword $config SCHEDULE | cut -d' ' -f1)
}

function modules_get_schedule_event {
  local config=$1
  echo $(filter_config_by_keyword $config SCHEDULE | cut -d' ' -f2)
}

function modules_get_update_events {
  local config=$1
  echo $(filter_config_by_keyword $config UPDATE_ON)
}
