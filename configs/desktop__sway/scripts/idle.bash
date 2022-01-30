#!/bin/sh
#==============================================================================
# IDLE
#==============================================================================

# Script that triggers the hibernation if needed.

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# HELP
#==============================================================================

#==============================================================================
# Prints out the help message.
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

    ${BOLD}my-sway-idle${RESET} - helper script to handle the idle states

    This helper script is intended to be triggered by the swayidle tool with
    the approprate timeout messages. The script will decide on what to do based
    on the computer current state (on charger or not).

${BOLD}SYNOPSYS${RESET}

    ${BOLD}my-sway-idle timeout <minutes>${RESET}
    ${BOLD}my-sway-idle resume <minutes>${RESET}
    ${BOLD}my-sway-idle lock${RESET}
    ${BOLD}my-sway-idle unlock${RESET}
    ${BOLD}my-sway-idle before-sleep${RESET}
    ${BOLD}my-sway-idle after-resume${RESET}
    ${BOLD}dm-sway-idle [--help|-h] ${RESET}

${BOLD}DESCRIPTION${RESET}

    ${BOLD}timeout <minutes>${RESET}
    Should be called after the given <minutes> idle time has reached.

    ${BOLD}resume <minutes>${RESET}
    It will be called after the corresponding [timeout] event is called and
    there were activity on the device. Could be useful to undo the actions the
    [timeout] event did.

    ${BOLD}lock${RESET}
    Should be called before the lock action.

    ${BOLD}unlock${RESET}
    Should be called after the unlock action.

    ${BOLD}before-sleep${RESET}
    Should be called before the sleep event.

    ${BOLD}after-resume${RESET}
    Should be called after the wake event.

${BOLD}OPTIONS${RESET}

    ${BOLD}my-sway-lock [--help|-h]${RESET}
    Prints out this help message and quit.

${BOLD}AUTHOR${RESET}

    ${BOLD}Tibor Simon${RESET} - 2021-05.
END
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
log() {
  message="$1"
  echo "$message" | systemd-cat -p info -t my-sway-idle
}

#==============================================================================
# ENTRY POINT
#==============================================================================

report_invalid_parameters() {
  ___reason="$1"
  ___details="$2"
  echo "$___reason"
  echo "$___details"
  exit 1
}

while [ "$#" -gt '0' ]
do
  case "$1" in
    timeout )
      if [ "$#" -lt '2' ]
      then
        report_invalid_parameters \
          'Insufficient [timeout] parameter count!' \
          'The [timeout] requires a <minutes> parameter.'
      fi
      minutes="$2"
      log "Event received: '$1 $minutes'"
      case "$minutes" in
        5 )
          log 'Triggering lockscreen..'
          my-sway-lock slow
          ;;
        15 )
          # log 'Turning off the screen..'
          # swaymsg 'output * dpms off'
          ;;
        20 )
          log 'Triggering hibernating..'
          my-machine-hibernate
          ;;
        * )
          ;;
      esac
      exit 0
      ;;
    resume )
      if [ "$#" -lt '2' ]
      then
        report_invalid_parameters \
          'Insufficient [resume] parameter count!' \
          'The [resume] requires a <minutes> parameter.'
      fi
      minutes="$2"
      log "Event received: '$1 $minutes'"
      case "$minutes" in
        15 )
          # log 'Turning back the screen..'
          # swaymsg 'output * dpms on'
          ;;
        * )
          ;;
      esac
      exit 0
      ;;
    lock )
      log "Event received: '$1'"
      my-sway-lock fast
      exit 0
      ;;
    unlock )
      log "Event received: '$1'"
      # log 'Turning back the screen..'
      # swaymsg "output * dpms on"
      exit 0
      ;;
    before-sleep )
      log "Event received: '$1'"
      my-sway-lock fast
      exit 0
      ;;
    after-resume )
      log "Event received: '$1'"
      # log 'Turning back the screen..'
      # swaymsg "output * dpms on"
      exit 0
      ;;
    --help|-h )
      display_help
      exit 0
      ;;
    * )
      display_help
      exit 0
      ;;
  esac
done

# Default action is to print out the help.
display_help
