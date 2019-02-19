#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

MODULES_PATH="./modules"
CLOCK=$(date +%R)


echo "Lemonbar Scheduler started."

# ===================================================================
#  RUNNING THE LEMONBAR UPDATE IN EVERY MINUTE
my-lemonbar-update


# ===================================================================
#  GATHERING SCHEDULE FILES

schedules=$(find ${MODULES_PATH} -type f -name schedule)


tasks=""

for schedule in $schedules
do
  content=$(cat $schedule)
  for line in $content
  do
    schedule_pattern=$(echo $line | cut -d'=' -f1)
    file=$(echo $line | cut -d'=' -f2)
    script="$(dirname $schedule)/$file"
    module="$(basename $(dirname $schedule))"

    if echo $CLOCK | grep -qP "$schedule_pattern"
    then
      tasks="$tasks $script=$module=$file"
    fi
  done
done


# ===================================================================
#  RUNNING SCHEDULED TASKS

if (( ${#tasks} == 0 ))
then
  echo "[ .. ] No task was scheduled at $CLOCK. Nothing to do.."
else
  for task in $tasks
  do
    script=$(echo $task | cut -d'=' -f1)
    module=$(echo $task | cut -d'=' -f2)
    file=$(echo $task | cut -d'=' -f3)

    echo "[ >> ][$module/$file] is scheduled at $CLOCK."
    $script | sed -e "s/^/[ .. ][$module\/$file] /"
  done

  # update lemonbar with the fresh data
  my-lemonbar-update
fi

echo "Lemonbar Scheduler finished."

