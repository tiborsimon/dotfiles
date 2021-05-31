#!/bin/sh
#==============================================================================                   
# SWAY - SCRATCHPAD HANDLER
#==============================================================================

# The reason of this script is the odd default behavior of the scratchpad show
# command. If we are in a fullscreen window, and we invoke the scartchpad, the
# fullscreen mode will disappear after we hide the scratchpad. This could be
# very annoying if you expect to see the previously set seen screen.

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
#  _____      _ _                                __  __           _
# |  ___|   _| | |___  ___ _ __ ___  ___ _ __   |  \/  | ___   __| | ___
# | |_ | | | | | / __|/ __| '__/ _ \/ _ \ '_ \  | |\/| |/ _ \ / _` |/ _ \
# |  _|| |_| | | \__ \ (__| | |  __/  __/ | | | | |  | | (_) | (_| |  __/
# |_|   \__,_|_|_|___/\___|_|  \___|\___|_| |_| |_|  |_|\___/ \__,_|\___|
#==============================================================================
# FULLSCREEN MODE HANDLING
#==============================================================================

DM_STORE__KEY__FULLSCREEN_MODE='my-sway-scratchpad__fullscreen_mode'

FULLSCREEN_MODE__ON='1'
FULLSCREEN_MODE__OFF='0'

#==============================================================================
# If there was no saved fullscreen mode, we can assume that there were no
# fullscreen mode.
#------------------------------------------------------------------------------
# Globals:
#   DM_STORE__KEY__FULLSCREEN_MODE
#   FULLSCREEN_MODE__OFF
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
init_fullscreen_mode() {
  if ! dm-store get "$DM_STORE__KEY__FULLSCREEN_MODE" >/dev/null
  then
    dm-store set "$DM_STORE__KEY__FULLSCREEN_MODE" "$FULLSCREEN_MODE__OFF"
  fi
}

#==============================================================================
# Function that should determine if the original window was in fullscreen mode
# or not.
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
#   0 - Original window was in fullscreen mode.
#   1 - Original window was not in fullscreen mode.
#   2 - Error happened during execution.
#==============================================================================
in_fullscreen_mode() {
  if result="$( \
    swaymsg -t get_tree | \
    jq --raw-output '..|try select(.focused == true) | .fullscreen_mode' \
  )"
  then
    if [ "$result" = '1' ]
    then
      return 0
    else
      return 1
    fi
  else
    return 2
  fi
}

#==============================================================================
# Returns true if the original window was in fullscreen mode.
#------------------------------------------------------------------------------
# Globals:
#   DM_STORE__KEY__FULLSCREEN_MODE
#   FULLSCREEN_MODE__ON
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
#   0 - Original window was in fullscreen mode.
#   1 - Original window was not in fullscreen mode.
#   2 - Error happened during execution.
#==============================================================================
was_fullscreen_mode() {
  if result="$(dm-store get "$DM_STORE__KEY__FULLSCREEN_MODE")"
  then
    if [ "$result" = "$FULLSCREEN_MODE__ON" ]
    then
      return 0
    else
      return 1
    fi
  else
    return 2
  fi
}

#==============================================================================
# Saves the full screen mode.
#------------------------------------------------------------------------------
# Globals:
#   DM_STORE__KEY__FULLSCREEN_MODE
#   FULLSCREEN_MODE__ON
#   FULLSCREEN_MODE__OFF
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
#   0 - Fullscreen mode was saved successfully.
#   1 - Error happened during save.
#==============================================================================
save_fullscreen_mode() {
  if in_fullscreen_mode
  then
    dm-store set "$DM_STORE__KEY__FULLSCREEN_MODE" "$FULLSCREEN_MODE__ON"
  else
    dm-store set "$DM_STORE__KEY__FULLSCREEN_MODE" "$FULLSCREEN_MODE__OFF"
  fi
}

#==============================================================================
#  ____            _       _                     _   ____  _        _
# / ___| _ __ __ _| |_ ___| |__  _ __   __ _  __| | / ___|| |_ __ _| |_ ___
# \___ \| '__/ _` | __/ __| '_ \| '_ \ / _` |/ _` | \___ \| __/ _` | __/ _ \
#  ___) | | | (_| | || (__| | | | |_) | (_| | (_| |  ___) | || (_| | ||  __/
# |____/|_|  \__,_|\__\___|_| |_| .__/ \__,_|\__,_| |____/ \__\__,_|\__\___|
#===============================|_|============================================
# SCRATCHPAD STATE HANDLING
#==============================================================================

DM_STORE__KEY__SCRATCHPAD_STATE='my-sway-scratchpad__scratchpad_state'

SCRATCHPAD_STATE__IS_HIDDEN='0'
SCRATCHPAD_STATE__IS_VISIBLE='1'

#==============================================================================
# If there was no saved scratchpad state, we can assume that the scratchpad is
# not active
#------------------------------------------------------------------------------
# Globals:
#   DM_STORE__KEY__SCRATCHPAD_STATE
#   SCRATCHPAD_STATE__IS_HIDDEN
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
init_scratchpad_state() {
  if ! dm-store get "$DM_STORE__KEY__SCRATCHPAD_STATE" >/dev/null
  then
    dm-store set "$DM_STORE__KEY__SCRATCHPAD_STATE" "$SCRATCHPAD_STATE__IS_HIDDEN"
  fi
}

#==============================================================================
# Interrogates the store and gets the current state.
#------------------------------------------------------------------------------
# Globals:
#   DM_STORE__KEY__SCRATCHPAD_STATE
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
#   0 - State returned.
#   1 - Error during operation.
#==============================================================================
get_scratchpad_state() {
  if result="$(dm-store get "$DM_STORE__KEY__SCRATCHPAD_STATE")"
  then
    echo "$result"
  else
    return 1
  fi
}

#==============================================================================
# Saves the given scratchpad state
#------------------------------------------------------------------------------
# Globals:
#   DM_STORE__KEY__SCRATCHPAD_STATE
# Arguments:
#   [1] scratchpad_state - Scratchpad state that should be saved to the store.
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
#   0 - State saved.
#   1 - Error during operation.
#==============================================================================
save_scratchpad_state() {
  state="$1"
  if ! dm-store set "$DM_STORE__KEY__SCRATCHPAD_STATE" "$state"
  then
    return 1
  fi
}

#==============================================================================
# Handles the hidden state -> It should save the current fullscreen mode and
# invoke the scratchpad.
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
#   0 - State returned.
#   1 - Error during operation.
#==============================================================================
activate_scratchpad() {
  save_fullscreen_mode
  swaymsg scratchpad show
  save_scratchpad_state "$SCRATCHPAD_STATE__IS_VISIBLE"
}

#==============================================================================
# Handles the visible state -> It should hide the scratchpad and restore the
# fullscreen mode.
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
#   0 - State returned.
#   1 - Error during operation.
#==============================================================================
hide_scratchpad() {
  swaymsg scratchpad show
  save_scratchpad_state "$SCRATCHPAD_STATE__IS_HIDDEN"
  if was_fullscreen_mode
  then
    swaymsg fullscreen
  fi
}

#==============================================================================
# ENTRY POINT
#==============================================================================

init_fullscreen_mode
init_scratchpad_state

current_state="$(get_scratchpad_state)"

case "$current_state" in
  "$SCRATCHPAD_STATE__IS_HIDDEN" )
    activate_scratchpad
    ;;
  "$SCRATCHPAD_STATE__IS_VISIBLE" )
    hide_scratchpad
    ;;
  * )
    exit 1
    ;;
esac
