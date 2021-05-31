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
LOCKSCREEN_RING_COLOR='ebd83444'
LOCKSCREEN_FILL_COLOR='ebd83400'
LOCKSCREEN_TEXT_COLOR='ebd83488'

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

    ${BOLD}my-sway-lock${RESET} - wrapper around the swaylock tool

    With this script you can trigger fast and slow locking action with grace
    time to interrupt the locking event.

${BOLD}SYNOPSYS${RESET}

    ${BOLD}my-sway-lock fast${RESET}
    ${BOLD}my-sway-lock slow${RESET}
    ${BOLD}my-sway-lock [--help|-h] ${RESET}

${BOLD}DESCRIPTION${RESET}

    ${BOLD}fast${RESET}
    Locks the screen with the fast locking action. The locking action can be
    interrupted with any button or mouse movement. This action is better suited
    for the intended locking with a button.

    ${BOLD}slow${RESET}
    Locks the screen with the slow locking action. The locking action can be
    interrupted with any button or mouse movement. This action is better suited
    for an automatick locking after an idle timeout occured. As it locking
    slowly it is easy to interrupt if you don't want the machine to get locked.

${BOLD}OPTIONS${RESET}

    ${BOLD}[--help|-h]${RESET}
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
  echo "$message" | systemd-cat -p info -t my-sway-lock
}

#==============================================================================
# LOCK TRIGGERING
#==============================================================================

#==============================================================================
# Common lock call.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] timeout - Time to lock the screen: fade-in and grace time.
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
common_lock_triggering() {
  timeout="$1"
  swaylock \
    --daemonize \
    --show-failed-attempts \
    --screenshot \
    --effect-blur '10x10' \
    --effect-vignette '0.5:0.9' \
    --indicator-thickness '7' \
    --indicator-radius '52' \
    --indicator-idle-visible \
    --line-uses-ring \
    --key-hl-color "$LOCKSCREEN_RING_COLOR" \
    --bs-hl-color "$LOCKSCREEN_RING_COLOR" \
    --ring-color "$LOCKSCREEN_RING_COLOR" \
    --ring-clear-color "$LOCKSCREEN_RING_COLOR" \
    --ring-ver-color "$LOCKSCREEN_RING_COLOR" \
    --ring-wrong-color "$LOCKSCREEN_RING_COLOR" \
    --inside-color "$LOCKSCREEN_FILL_COLOR" \
    --inside-clear-color "$LOCKSCREEN_FILL_COLOR" \
    --inside-ver-color "$LOCKSCREEN_FILL_COLOR" \
    --inside-wrong-color "$LOCKSCREEN_FILL_COLOR" \
    --text-color "$LOCKSCREEN_TEXT_COLOR" \
    --text-clear-color "$LOCKSCREEN_TEXT_COLOR" \
    --text-ver-color "$LOCKSCREEN_TEXT_COLOR" \
    --text-wrong-color "$LOCKSCREEN_TEXT_COLOR" \
    --fade-in "$timeout" \
    --grace "$timeout"
}
#==============================================================================
# Triggers the slow locking action.
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
trigger_slow_lock() {
  log "Slow locking action has been triggered"
  common_lock_triggering '8'
}

#==============================================================================
# Triggers the fast locking action.
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
trigger_fast_lock() {
  log "Fast locking action has been triggered"
  common_lock_triggering '0.5'
}

#==============================================================================
# ENTRY POINT
#==============================================================================

while [ "$#" -gt '0' ]
do
  case "$1" in
    slow )
      trigger_slow_lock
      exit 0
      ;;
    fast )
      trigger_fast_lock
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
