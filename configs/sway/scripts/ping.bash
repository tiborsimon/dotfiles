#!/bin/sh
#==============================================================================
# SWAY - LOCK
#==============================================================================

# Script that triggers the hibernation if needed.

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

    ${BOLD}my-sway-ping${RESET} - waybar compatible ping time module

    Small script that executes a ping command to check the network latency of
    the current  connection.

${BOLD}SYNOPSYS${RESET}

    ${BOLD}my-sway-ping measure${RESET}
    ${BOLD}my-sway-ping check${RESET}
    ${BOLD}my-sway-ping [--help|-h] ${RESET}

${BOLD}DESCRIPTION${RESET}

    ${BOLD}measure${RESET}
    Measures the network latency and outputs the result in a waybar compatible way.


    ${BOLD}check${RESET}
    Checks if it is reasonable or not to run the measure command i.e. is there
    an established connection to run the measurement through.

${BOLD}OPTIONS${RESET}

    ${BOLD}[--help|-h]${RESET}
    Prints out this help message and quit.

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
  echo "$message" | systemd-cat -p err -t my-sway-ping
}

#==============================================================================
# HANDLERS
#==============================================================================


#==============================================================================
# Handler for the measurement mode.
#------------------------------------------------------------------------------
# Globals:
#   PING_DESTINATION
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
handle_measure() {
  if result="$(ping -c 4 "$PING_TARGET" 2>&1)"
  then
    measurement="$(
      echo "$result" | \
      tail -1 | \
      awk '{print $4}' | \
      cut -d '/' -f 2 | \
      xargs -I {} printf '%.1fms' {} \
    )"
    echo "{\"class\": \"connected\", \"text\": \"${measurement}\"}"
  else
    log_error "Error during measurement: '${result}'"
    echo "{\"class\": \"error\", \"text\": \"-\"}"
  fi
}

#==============================================================================
# Handler for the checking mode.
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
#   0 - Networking has full connectivity.
#   1 - Networking has linited connectivity.
#==============================================================================
handle_check() {
  # The nmcli call can also fail here.
  result="$(nmcli networking connectivity check)"
  test "$result" == 'full'
}

#==============================================================================
# ENTRY POINT
#==============================================================================

while [ "$#" -gt '0' ]
do
  case "$1" in
    measure )
      handle_measure
      exit 0
      ;;
    check )
      handle_check
      exit "$?"
      ;;
    --help|-h )
      display_help
      ;;
    * )
      display_help
      ;;
  esac
done

# Default action is to print out the help.
display_help
