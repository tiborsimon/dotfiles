#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

echo "[ >> ][scheduler] started."

source ~/.config/lemon-modules/config.bash
source ./module-utils.bash


# ===================================================================
#  GATHERING SCHEDULED EVENTS

module_config_files=$(modules_get_config_file_list)

tasks=""
current_time=$(date +%R)

for module_config_file in ${module_config_files}
do
  pattern=$(modules_get_schedule_pattern $module_config_file)

  if [ -z "$pattern" ]
  then
    continue
  fi

  priority=$(modules_get_priority $module_config_file)
  event=$(modules_get_schedule_event $module_config_file)

  if echo ${current_time} | grep -qP "${pattern}"
  then
    tasks="${tasks} ${priority},${event}"
  fi
done


# ===================================================================
#  RUNNING SCHEDULED TASKS

if (( ${#tasks} == 0 ))
then
  echo "[ .. ][scheduler] No task was scheduled at ${CLOCK}. Nothing to do.."
else
  tasks=$(echo ${tasks} | tr " " "\n" | sort)
  for task in ${tasks}
  do
    event=$(echo ${task} | cut -d',' -f2)

    $UPDATE_COMMAND --event ${event}

    echo "[ .. ][scheduler] <${event}> event fired."

  done
fi

echo "[ ok ][scheduler] done."

