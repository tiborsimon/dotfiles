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

# Switching to the script's location.
cd $(dirname $(readlink -f $0))

# Getting the location of the separated install scripts.
scripts=$(find . -mindepth 2 -type f -name install-aur.bash | sort)

# Running through the list of install script locations..
for script in $scripts; do
  # Getting the file's directory path.
  dir=$(dirname $script)

  # Jumping to that directory through the directory stack.
  pushd $dir &>/dev/null

  # Run the AUR install script in the current shell.
  ./install-aur.bash

  # Jumping back to the directory stack.
  popd &>/dev/null
done
