#!/usr/bin/env bash
#######################################
# Installs dependent packages located in AUR. It calls the separated AUR
# install script in the current shell.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################

CUSTOM_INSTALL_SCRIPT_NAME='install-custom.bash'

# Switching to the script's location.
cd $(dirname $(readlink -f $0))

# Getting the location of the separated install scripts.
scripts=$(find . -mindepth 2 -type f -name ${CUSTOM_INSTALL_SCRIPT_NAME} | sort)

# Running through the list of install script locations..
for script in $scripts; do
  # Getting the file's directory path.
  dir=$(dirname $script)

  # Jumping to that directory through the directory stack.
  pushd $dir &>/dev/null

  # Run the AUR install script in the current shell.
  ./${CUSTOM_INSTALL_SCRIPT_NAME}

  # Jumping back to the directory stack.
  popd &>/dev/null
done
