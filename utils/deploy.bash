#!/usr/bin/env bash
#######################################
# Global dotfiles deployment script. It calls all subsystem's deployment
# scripts.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################

# Switching to the script's location.
cd $(dirname $(readlink -f $0))

# Calling the script deployment.
../scripts/deploy.bash

# Calling the config deployment.
../configs/deploy.bash
