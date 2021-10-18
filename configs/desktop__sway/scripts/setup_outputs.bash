#!/bin/sh
#==============================================================================
# SWAY - SETUP OUTPUTS
#==============================================================================

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# GLOBAL VARIABLES
#==============================================================================
PING_TARGET='1.1.1.1'

#==============================================================================
# HELP
#==============================================================================

#==============================================================================
# Prints out the help message then exits.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   None
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   None
# STDERR:
#   None
# Status:
#   0 - Other status is not expected.
#==============================================================================
display_help() {
  RESET="$(tput sgr0)"
  BOLD="$(tput bold)"
  cat <<-END

${BOLD}NAME${RESET}

    ${BOLD}my-sway-setup_outputs${RESET} - script that setups the outputs

    External screens could have dynamic names between reboots/wakes, so this
    script gets the names dynamically and sets up the layout.

${BOLD}SYNOPSYS${RESET}

    ${BOLD}my-sway-setup_outputs${RESET}

${BOLD}AUTHOR${RESET}

    ${BOLD}Tibor Simon${RESET} - 2021-06.
END
  exit 0
}

#==============================================================================
# LOGGING
#==============================================================================

#==============================================================================
# Sends a log message to the systemd yournal with the appropriate settings.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] message - Message to send to the journal.
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   None
# STDERR:
#   None
# Status:
#   0 - Other status is not expected.
#==============================================================================
log_error() {
  message="$1"
  echo "$message" | systemd-cat -p err -t my-sway-setup_outputs
}

#==============================================================================
# ENTRY POINT
#==============================================================================

LAPTOP_SCREEN_NAME='eDP-1'

EXTERNAL_MONITOR_MODEL='24MP76'
EXTERNAL_MONITOR_NAME="$( \
  swaymsg -t get_outputs | \
  jq -r "..|try select(.model == \"${EXTERNAL_MONITOR_MODEL}\") | .name" \
)"

swaymsg output "$LAPTOP_SCREEN_NAME" pos 0 1000
swaymsg output "$EXTERNAL_MONITOR_NAME" pos 1920 0 transform 0
