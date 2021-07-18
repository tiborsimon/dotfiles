#!/bin/sh
#==============================================================================
# SWAY - FIREJAIL
#==============================================================================

# Script that tells the currently running firejail targets.

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

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

    ${BOLD}my-sway-firejail${RESET} - waybar compatible firejail counter

    Small script that counts the currently firejail sandboxed programs.

${BOLD}SYNOPSYS${RESET}

    ${BOLD}my-sway-firejail display${RESET}
    ${BOLD}my-sway-firejail check${RESET}
    ${BOLD}my-sway-firejail [--help|-h] ${RESET}

${BOLD}DESCRIPTION${RESET}

    ${BOLD}display${RESET}
    Counts and displays the currently running sandboxed programs.


    ${BOLD}check${RESET}
    Checks if the command would be able to run.

${BOLD}OPTIONS${RESET}

    ${BOLD}[--help|-h]${RESET}
    Prints out this help message and quit.

${BOLD}AUTHOR${RESET}

    ${BOLD}Tibor Simon${RESET} - 2021-07.
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
  echo "$message" | systemd-cat -p err -t my-sway-firejail
}

#==============================================================================
# HANDLERS
#==============================================================================


#==============================================================================
# Handler for the display mode.
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
#   Waybar compatible json output.
# STDERR:
#   None
# Status:
#   0 - Other status is not expected.
#==============================================================================
handle_display() {
  if count="$(firejail --list 2>/dev/null | wc --lines)"
  then
    tooltip="$(
      firejail --list 2>/dev/null | while read -r line
      do
        echo -n "${line}\n"
      done
    )"
    # removing trailing new line sequence
    tooltip="$(echo "$tooltip" | sed 's/\\n$//')"
    echo "{\"class\": \"connected\", \"text\": \"Firejail ${count}\", \"tooltip\": \"${tooltip}\"}"
  else
    log_error "Error during display: '${count}'"
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
#   0 - Firejail is available.
#   1 - Firejail is not available.
#==============================================================================
handle_check() {
  command -v firejail 2>&1 1>/dev/null
}

#==============================================================================
# ENTRY POINT
#==============================================================================

while [ "$#" -gt '0' ]
do
  case "$1" in
    display )
      handle_display
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
