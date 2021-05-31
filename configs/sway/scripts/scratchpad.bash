#!/bin/sh
#==============================================================================
# SWAY - SCRATCHPAD
#==============================================================================

#==============================================================================
# SANE ENVIRONMENT
#==============================================================================

set -e  # exit on error
set -u  # prevent unset variable expansion

#==============================================================================
# GLOBAL VARIABLES
#==============================================================================

CACHE_DIR="${HOME}/.cache/my-sway-scratchpad-cache"

mkdir -p "$CACHE_DIR"

CACHE_FILE__ORIGINAL_WORKSPACE="${CACHE_DIR}/original_workspace.cache"
CACHE_FILE__ACTIVE_SCRATCHPAD="${CACHE_DIR}/active_scratchpad.cache"

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

    ${BOLD}my-sway-scratchpad${RESET} - custom scratchpad implementation

    Simple scratchpad implementation with named workspaces. Since the original
    sway scratchpad implementation lacks many features, this script should be
    used for multiple named scratchpads.

    This script uses the name 'workspace_name' and 'scratchpad_name' too. In
    general a 'scratchpad_name' is a 'workspace_name' but it has a non-numeric
    value. General workspaces has integer based names. Those are the workspaces
    that you work on in general. Scratchpads are workspaces that you can toggle
    to have something done quickly than you toggle back to the original
    numbered workspace.

${BOLD}SYNOPSYS${RESET}

    ${BOLD}my-sway-scratchpad toggle <scratchpad_name>${RESET}
    ${BOLD}my-sway-scratchpad invalidate${RESET}
    ${BOLD}my-sway-scratchpad [--help|-h] ${RESET}

${BOLD}DESCRIPTION${RESET}

    ${BOLD}toggle <scratchpad_name>${RESET}
    Switches to the workspace named by the given name. Before the switching,
    the script will save the currently active numbered workspace name and the
    given target scratchpad name too. Later on if the same scratchpad name
    received for the [toggle] command, that means that the script should switch
    back to the saved original numbered workspace. However, if another
    scratchpad name is received other than the saved one, that means, that the
    user decided to go to another scratchpad. Therefore the script schould
    switch to the new scratchpad, and overwrite the saved scratchpad name, to
    be prepared to the next decision point.

    ${BOLD}invalidate${RESET}
    Completely deletes the saved values. This should be called, if a manual
    next/previous workspace action is called, or a direct numbered workspace
    switch is executed. This is needed, because these kind of direct workspace
    switches would mess up the toggle logic.

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
  echo "$message" | systemd-cat -p info -t my-sway-scratchpad
}

#==============================================================================
# INTERNAL FUNCTIONS
#==============================================================================

#==============================================================================
# Prints the currently focused workspace name.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__ORIGINAL_WORKSPACE
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
save_current_workspace_name() {
  swaymsg -t get_workspaces | \
    jq -r '..|try select(.focused == true) | .name' > \
    "$CACHE_FILE__ORIGINAL_WORKSPACE"
}

#==============================================================================
# Prints out the previously focused workspace name.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__ORIGINAL_WORKSPACE
# Arguments:
#   None
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   Focused workspace name if exists.
# STDERR:
#   None
# Status:
#   0 - Workspace name printed.
#   1 - No workspace name stored.
#==============================================================================
get_original_workspace_name() {
  cat "$CACHE_FILE__ORIGINAL_WORKSPACE"
}

#==============================================================================
# Saves the given scratchpad name to the cache.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__ACTIVE_SCRATCHPAD
# Arguments:
#   [1] scratchpad_name - Scratchpad name that should be saved.
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
save_scratchpad_name() {
  scratchpad_name="$1"
  echo "$scratchpad_name" > "$CACHE_FILE__ACTIVE_SCRATCHPAD"
}

#==============================================================================
# Prints out the active saved scratchpad name if it is present.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__ACTIVE_SCRATCHPAD
# Arguments:
#   None
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   Focused workspace name if exists.
# STDERR:
#   None
# Status:
#   0 - Active scratchpad name printed.
#   1 - No active scratchpad name saved.
#==============================================================================
get_active_scratchpad_name() {
  cat "$CACHE_FILE__ACTIVE_SCRATCHPAD"
}

#==============================================================================
# Invalidates the cache by deleting the cache files.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__ORIGINAL_WORKSPACE
#   CACHE_FILE__ACTIVE_SCRATCHPAD
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
invalidate_cache() {
  rm -f "$CACHE_FILE__ORIGINAL_WORKSPACE"
  rm -f "$CACHE_FILE__ACTIVE_SCRATCHPAD"
}

#==============================================================================
# Switches to the given workspace.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] workspace_name - Generic workspace name that could be a numbered
#       workspace or a named scratchpad workspace.
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
switch_to_workspace() {
  workspace_name="$1"
  swaymsg workspace "$workspace_name"
}

#==============================================================================
# HANDLERS
#==============================================================================

#==============================================================================
# Handles the toggle event. It should support both toggling and replacing
# scratchpads for a better user experience.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] new_scratchpad_name - Scratchpad name that should be toggled.
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   Focused workspace name.
# STDERR:
#   None
# Status:
#   0 - Other status is not expected.
#==============================================================================
handle_toggle() {
  new_scratchpad_name="$1"

  if original_workspace_name="$(get_original_workspace_name)"
  then

    # If the active scratchpad name is different than the currently received
    # one, the script should switch to the new scratchpad instead of switching
    # back to the original workspace.
    if active_scratchpad_name="$(get_active_scratchpad_name)"
    then
      if [ "$new_scratchpad_name" != "$active_scratchpad_name" ]
      then
        log "Replacing active scratchpad '${active_scratchpad_name}' with new scratchpad '${new_scratchpad_name}'.."
        save_scratchpad_name "$new_scratchpad_name"
        switch_to_workspace "$new_scratchpad_name"
        return
      fi
    fi

    # Otherwise if the same scratchpad name is received, the script should
    # toggle back to the original workspace.
    log "Switching back to the original workspace.."
    invalidate_cache
    switch_to_workspace "$original_workspace_name"

  else

    log "Toggling from numbered workspace to scratchpad '${new_scratchpad_name}'.."
    save_current_workspace_name
    save_scratchpad_name "$new_scratchpad_name"
    switch_to_workspace "$new_scratchpad_name"

  fi
}

#==============================================================================
# Invalidates the return path if present.
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
#   Focused workspace name.
# STDERR:
#   None
# Status:
#   0 - Other status is not expected.
#==============================================================================
handle_invalidate() {
  invalidate_cache
  log "Cache invalidated due to manual workspace switching."
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
    toggle )
      if [ "$#" -lt '2' ]
      then
        report_invalid_parameters \
          'Insufficient [toggle] parameter count!' \
          'The [toggle] operation requires a <scratchpad_name>.'
      fi
      scratchpad_name="$2"
      handle_toggle "$scratchpad_name"
      exit 0
      ;;
    invalidate )
      handle_invalidate
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
