#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

MODULES_PATH="./modules"
MODULE_SCHEDULE_FILE="module_schedule"
CLOCK=$(date +%R)

# ====================================================================
#  P A R A M E T E R   P A R S I N G

HELP=false

while [[ $# -gt 0 ]]
do
key="$1"

case ${key} in
  -h|--help)
    HELP=true
    shift
    ;;
  *)
    warning "Invalid parameter: ${BOLD}${RED}${key}${RESET}"
    shift
    ;;
esac
done


# ====================================================================
#  H E L P   C O M M A N D

if [ ${HELP} == true ]
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
  echo -e "\n${help_message}"
  exit 0
fi

echo "Lemonbar Scheduler started."


# ===================================================================
#  GATHERING SCHEDULE FILES

schedule_files=$(find ${MODULES_PATH} -type f -name ${MODULE_SCHEDULE_FILE})

tasks=""

for schedule_file in ${schedule_files}
do
  content=$(cat ${schedule_file})
  for line in ${content}
  do
    priority=$(echo ${line} | cut -d',' -f1)
    pattern=$(echo ${line} | cut -d',' -f2)
    event=$(echo ${line} | cut -d',' -f3)
    target_script=$(echo ${line} | cut -d',' -f4)

    script_path="$(dirname ${schedule_file})/${target_script}"
    module="$(basename $(dirname ${schedule_file}))"

    if echo ${CLOCK} | grep -qP "${pattern}"
    then
      tasks="${tasks} ${priority},${module},${event},${target_script},${script_path}"
    fi
  done
done


# ===================================================================
#  RUNNING SCHEDULED TASKS

if (( ${#tasks} == 0 ))
then
  echo "[ .. ] No task was scheduled at ${CLOCK}. Nothing to do.."
else
  tasks=$(echo ${tasks} | tr " " "\n" | sort)
  for task in ${tasks}
  do
    module=$(echo ${task} | cut -d',' -f2)
    event=$(echo ${task} | cut -d',' -f3)
    target_script=$(echo ${task} | cut -d',' -f4)
    script_path=$(echo ${task} | cut -d',' -f5)

    if [ "${target_script}" != "-" ]
    then
      echo "[ >> ][${module}/${target_script}] is scheduled at ${CLOCK}."
      if ${script_path} | sed -e "s/^/[ .. ][${module}\/${target_script}] > /"
      then
        my-lemonbar-update --event ${event}
        echo "[ ok ][${module}/${target_script}] finished, event <${event}> fired."
      else
        echo "[ !! ][${module}/${target_script}] failed."
      fi
    else
      echo "[ >> ][${module}] is scheduled at ${CLOCK} in event only mode."
      my-lemonbar-update --event ${event}
      echo "[ >> ][${module}] event <${event}> fired."
    fi
  done
fi

echo "Lemonbar Scheduler finished."

