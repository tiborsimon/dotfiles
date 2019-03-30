#!/usr/bin/bash

if [ -z ${MODULES_PATH+x} ]
then
  echo "[ !! ][module-utils] Source the config file before the module-utils file!"
  exit 1
fi

function get_module_list {
  find $MODULES_PATH -mindepth 1 -maxdepth 1 -type d
}

function modules_get_position {
  local module=$1
  local target_file="${module}/${MODULE_POSITION_FILE_NAME}"
  if [ -f $target_file ]
  then
    cat $target_file
  fi
}

function modules_get_priority {
  local module=$1
  local target_file="${module}/${MODULE_PRIORITY_FILE_NAME}"
  if [ -f $target_file ]
  then
    cat $target_file
  fi
}

function modules_get_schedule_pattern {
  local module=$1
  local target_file="${module}/${MODULE_SCHEDULE_FILE_NAME}"
  if [ -f $target_file ]
  then
    cat $target_file | cut -d' ' -f1
  fi
}

function modules_get_schedule_event {
  local module=$1
  local target_file="${module}/${MODULE_SCHEDULE_FILE_NAME}"
  if [ -f $target_file ]
  then
    cat $target_file | cut -d' ' -f2
  fi
}

function modules_get_update_events {
  local module=$1
  local target_file="${module}/${MODULE_UPDATE_ON_FILE_NAME}"
  if [ -f $target_file ]
  then
    cat $target_file
  fi
}
