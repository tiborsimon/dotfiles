#!/bin/sh
#==============================================================================
# HIBERNATE
#==============================================================================

# Script that triggers the hibernation if needed.

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# INTERNAL FUNCTIONS
#==============================================================================

log() {
  message="$1"
  echo "$message" | systemd-cat -p info -t my-sway-hibernate
}

if my-machine-on_charger
then
  log 'Hibernation prevented as the machine is on a charger.'
else
  log 'Triggering hibernation as machine is running from the batteries.'
  systemctl hibernate
fi
