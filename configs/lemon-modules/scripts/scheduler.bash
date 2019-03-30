#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

echo "[ >> ][scheduler] started."

source ~/.config/lemon-modules/config.bash
source ./module-utils.bash


# ===================================================================
#  GATHERING SCHEDULED EVENTS

modules=$(get_module_list)

tasks=""
current_time=$(date +%R)

for module in ${modules}
do
  pattern=$(modules_get_schedule_pattern $module)

  if [ -z "$pattern" ]
  then
    continue
  fi

  priority=$(modules_get_priority $module)
  event=$(modules_get_schedule_event $module)

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

    echo "[ .. ][scheduler] <${event}> event fired."

    lemon-modules-update --event ${event} --debug 2>&1

  done
fi

echo "[ ok ][scheduler] done."
