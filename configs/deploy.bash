#!/usr/bin/env bash
#######################################
# Custom configuration deployment script. It locates the separated
# configuration bundles and runs the separate deployment scripts.
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

# Getting the list of the deployable configuration locations based on the
# presence of the local deployment script.
configs=$(find . -mindepth 2 -type f -name deploy.bash | sort)

# Running through the list of configurations.
for config in $configs; do
  # Getting the file's directory path.
  dir=$(dirname $config)

  # Jumping to that directory through the directory stack.
  pushd $dir &>/dev/null

  # Call the deployment script in that level.
  ./deploy.bash

  # Jumping back to the directory stack.
  popd &>/dev/null
done
