#!/usr/bin/env bash
#######################################
# Runs the installer scripts located in the packages.
# Globals:
#   None
# Arguments:
#   config - the given config will be installed
#   None   - all configs will be installed
# Returns:
#   None
#######################################
set -e

# Switching to the script's location.
cd $(dirname $(readlink -f $0))
source ./libdeploy.bash

cd ../configs

init_error_log
init_messages
trap clean_up_messages EXIT

if [ "$#" -eq 1 ]; then
  configs=$1
else
  # Getting the directory names taht contains the configurations
  configs=$(ls | grep -v '.bash' | grep -v Makefile | sort)
fi

for config in $configs; do
  if [ -f ./$config/retired ]; then
    echo "====#========================================================================"
    success "Skipped: ${BOLD}${config}${RESET} - ${YELLOW}config retired${RESET}"
    continue
  else
    echo "====#========================================================================"
    task "Installing: ${BOLD}${config}${RESET}"
    echo "----+------------------------------------------------------------------------"
  fi

  scripts=$(find $config -maxdepth 1 -type f -executable | sort)
  for script in $scripts; do
    write_to_error_log "Running script: ./configs/$script"
    info "Running script ${BOLD}${script}${RESET}"
    ./$script
  done
  success "Done"
done

display_messages

clean_up_error_log
clean_up_messages
