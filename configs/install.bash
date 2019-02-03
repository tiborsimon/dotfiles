#!/usr/bin/env bash
#######################################
# Runs the installer scripts located in the packages.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################

set -e

# Switching to the script's location.
cd $(dirname $(readlink -f $0))

# Logfile path in the repository root.
export DOTFILES_ERROR_LOG_PATH=$(readlink -f ../error.log)

source ../utils/libdeploy.bash

mkdir -p ${HOME}/.config
mkdir -p ${HOME}/.local/bin
mkdir -p ${HOME}/.scripts

init_error_log

if [ "$#" -eq 1 ]; then
  configs=$1
else
  # Getting the directory names taht contains the configurations
  configs=$(ls | grep -v '.bash' | grep -v Makefile | sort)
fi

for config in $configs; do
  echo "====#========================================================================"
  task "Installing: ${BOLD}${config}${RESET}"
  echo "----+------------------------------------------------------------------------"
  scripts=$(find $config -maxdepth 1 -type f -executable | sort)
  for script in $scripts; do
    write_to_error_log "Running script: ./configs/$script"
    info "Running script ${BOLD}${script}${RESET}"
    ./$script
  done
  success "Done"
done

clean_up_error_log
