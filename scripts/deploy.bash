#!/usr/bin/env bash
#######################################
# Custom script installation script. These scripts cannot be added to a
# concrete configuration therefore there are put together in categories in this
# separate script section.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################

# A prefix can be specified that will be prepended to the link names to easily
# be able to distinguish them from the system commands. This also helps to
# search between them.
PREFIX="my"

# The preferred location is the user's local directory. Make sure that your
# PATH contains this location.
LINK_PATH="${HOME}/.local/bin"


# Switching to the script's location.
cd $(dirname $(readlink -f $0))

# Use the deployment library.
source ../utils/lib/libdeploy.bash

# Making sure that the target path exists..
mkdir -p ${LINK_PATH}

# Getting the list of the deployable scripts.
scripts=$(find . -mindepth 2 -type f -executable | sort)

# Running through the list of files and link them to the destination.
for script_path in $scripts; do
  # Getting the name without the extension.
  script_name=$(basename $script_path | cut -d. -f1)

  # Getting the containing directory name as a category.
  script_category=$(basename $(dirname $script_path))

  # Assembling the script's full name that would be linked to.
  script_full_name="${PREFIX}-${script_category}-${script_name}"

  # Calling the library linker fuction.
  link_package scripts ${script_path} ${LINK_PATH}/${script_full_name}
done
