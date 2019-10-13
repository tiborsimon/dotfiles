#!/usr/bin/env bash
#######################################
# Generates make targets for the configurations.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################

TARGET_MAKEFILE='Makefile.targets'

set -e

BOLD=$(tput bold)
RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
RESET=$(tput sgr0)

cd $(dirname $(readlink -f $0))

configs=$(cd ../configs; ls | grep -v '.bash' | grep -v Makefile | sort)


function log_target {
  local retired_path="../configs/${config}/retired"
  if [ -f "$retired_path" ]
  then
    echo "[ .. ] ${BOLD}${BLUE}${config}${RESET} - ${BOLD}${YELLOW}retired${RESET}"
  else
    echo "[ .. ] ${BOLD}${BLUE}${config}${RESET}"
  fi
}

function add_target {
  config=$1
  echo ".PHONY: install-$config" >> $TARGET_MAKEFILE
  echo "install-$config:" >> $TARGET_MAKEFILE
  echo "	@./utils/install.bash $config" >> $TARGET_MAKEFILE
  echo "" >> $TARGET_MAKEFILE
}


echo "[ >> ] Looking for target configurations.."

rm -f $TARGET_MAKEFILE
for config in $configs; do
  log_target $config
  add_target $config
done

echo "[ ok ] Target list generated."
