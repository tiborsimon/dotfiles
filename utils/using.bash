#!/usr/bin/env bash
#######################################
# Helper function to be called before you call a third party tool that might
# not be installed on your system. This script will attempt to install it as
# the given tool installation method was previously implemented in the script.
# Globals:
#   None
# Arguments:
#   tool - 3rd party tool you want to use as a consequent command
# Returns:
#   0 - if the tool is already installed or just installed
#   1 - if the tool installation method was not found
#######################################

# Switching to the script's location.
pushd $(dirname $(readlink -f $0)) &> /dev/null

# Jumping back to the directory stack on exit.
trap "{ popd &>/dev/null }" EXIT

# Getting the installable tool's name.
tool=$1

# Temporary path to download the stuff.
TEMP_PATH='/tmp/dotfiles-sandbox'


#######################################
# YAY - the pacman like AUR installer
#######################################
if [ "$tool" = 'yay' ]; then
  if which yay&>/dev/null; then exit 0; fi
  echo "Installing yay.."

  mkdir -p ${TEMP_PATH}
  pushd ${TEMP_PATH} &> /dev/null

  if git clone https://aur.archlinux.org/yay.git; then
    cd yay
    makepkg -si
  fi

  popd &> /dev/null
  rm -rf ${TEMP_PATH}

  echo "yay installed."
  exit 0
fi

echo "ERROR [using]: tool '${tool}' installation was not implemented!"
exit 1
