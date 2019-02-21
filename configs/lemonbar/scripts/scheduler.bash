#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

MODULES_PATH="./modules"
CLOCK=$(date +%R)

# ====================================================================
#  P A R A M E T E R   P A R S I N G

HELP=false
LOGIN=false

while [[ $# -gt 0 ]]
do
key="$1"

case $key in
  -h|--help)
    HELP=true
    shift
    ;;
  --login)
    LOGIN=true
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

    if [ $LOGIN == true ]
    then
      if [ "$schedule_pattern" == "login" ]
      then
        tasks="$tasks $script=$module=$file"
      fi
    else
      if echo $CLOCK | grep -qP "$schedule_pattern"
      then
        tasks="$tasks $script=$module=$file"
      fi
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

