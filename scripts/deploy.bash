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

# Switching to the script's location.
cd $(dirname $(readlink -f $0))

# Use the deployment library.
source ../utils/lib/libdeploy.bash


# Getting the list of the deployable scripts.
scripts=$(find . -mindepth 2 -type f -executable | sort)

# Running through the list of files and link them to the destination.
for script_path in $scripts; do
  # Getting the containing directory name as a category.
  script_category=$(basename $(dirname $script_path))

  # Link the script
  link_scripts $script_category $script_path
done
