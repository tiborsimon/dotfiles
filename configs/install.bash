#!/usr/bin/env bash
#######################################
# Installs dependent packages returned by the the separated install scripts
# Each config bundle has to comply with the install API requirements. They
# has to print out the required packe names to be collected by this script.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################

DEPENDENCIES_SCRIPT_NAME='dependencies.bash'

# Switching to the script's location.
cd $(dirname $(readlink -f $0))

# Getting the location of the separated install scripts.
scripts=$(find . -mindepth 2 -type f -name ${DEPENDENCIES_SCRIPT_NAME} | sort)

# Initialize the empty packages list.
packages=()

# Running through the list of install script locations..
for script in $scripts; do
  # Getting the file's directory path.
  dir=$(dirname $script)

  # Jumping to that directory through the directory stack.
  pushd $dir &>/dev/null

  # Call the install script and append its output to the packages list.
  packages=("${packages[@]}" $(./${DEPENDENCIES_SCRIPT_NAME}))

  # Jumping back to the directory stack.
  popd &>/dev/null
done

# Install the collected packages.
sudo pacman -S ${packages[@]}
