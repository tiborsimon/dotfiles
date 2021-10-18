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
# HELP
#==============================================================================

#==============================================================================
# Prints out the help message and exits.
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
  exit 0
}

#==============================================================================
# LOGGING
#==============================================================================

#==============================================================================
# Sends a log message to the system journal.
#------------------------------------------------------------------------------
# Globals:
#   DEBUG_MODE
# Arguments:
#   [1] message - Log message to send to the journal.
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
# CACHE
#==============================================================================

CACHE_DIR="${HOME}/.cache/my-sway-scratchpad-cache"

# The workspace name the scratchpad action was invoked from.
CACHE_FILE__PRIMARY_WORKSPACE="${CACHE_DIR}/primary_workspace.cache"
# If the scratchpad was already created and it is located on a different output
# then the original workspace, that workspace should be restored before we
# restore the original workspace to "hide" the scratchpad as we should.
CACHE_FILE__SECONDARY_WORKSPACE="${CACHE_DIR}/secondary_workspace.cache"
# The name of the currently active scratchpad.
CACHE_FILE__ACTIVE_SCRATCHPAD="${CACHE_DIR}/active_scratchpad.cache"

#==============================================================================
# Initializes the cache system.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_DIR
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
cache__init() {
  mkdir -p "$CACHE_DIR"
}

#==============================================================================
# Returns true if the cache has content in it.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_DIR
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
#   0 - Cache has contnent.
#   1 - Cache has no content.
#==============================================================================
cache__has_content() {
  test -n "$(ls -A "$CACHE_DIR")"
}

#==============================================================================
# Invalidates the cache by deleting the cache files.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__PRIMARY_WORKSPACE
#   CACHE_FILE__SECONDARY_WORKSPACE
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
cache__invalidate() {
  rm -f "$CACHE_FILE__PRIMARY_WORKSPACE"
  rm -f "$CACHE_FILE__SECONDARY_WORKSPACE"
  rm -f "$CACHE_FILE__ACTIVE_SCRATCHPAD"
}

#==============================================================================
# Sets the value for the primary workspace.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__PRIMARY_WORKSPACE
# Arguments:
#   [1] value - Value that should be saved.
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
cache__set__primary_workspace() {
  value="$1"
  echo "$value" > "$CACHE_FILE__PRIMARY_WORKSPACE"
}

#==============================================================================
# Sets the value for the secondary workspace.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__SECONDARY_WORKSPACE
# Arguments:
#   [1] value - Value that should be saved.
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
cache__set__secondary_workspace() {
  value="$1"
  echo "$value" > "$CACHE_FILE__SECONDARY_WORKSPACE"
}

#==============================================================================
# Sets the value for the active scratchpad.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__ACTIVE_SCRATCHPAD
# Arguments:
#   [1] value - Value that should be saved.
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
cache__set__active_scratchpad() {
  value="$1"
  echo "$value" > "$CACHE_FILE__ACTIVE_SCRATCHPAD"
}

#==============================================================================
# Get the value from the primary workspace cache file if exists.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__PRIMARY_WORKSPACE
# Arguments:
#   None
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   Value from the cache file.
# STDERR:
#   None
# Status:
#   0 - Cache file is present and value returned.
#   1 - Cache file does not exist.
#==============================================================================
cache__get__primary_workspace() {
  cat "$CACHE_FILE__PRIMARY_WORKSPACE"
}

#==============================================================================
# Get the value from the secondary workspace cache file if exists.
#------------------------------------------------------------------------------
# Globals:
#   CACHE_FILE__SECONDARY_WORKSPACE
# Arguments:
#   None
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   Value from the cache file.
# STDERR:
#   None
# Status:
#   0 - Cache file is present and value returned.
#   1 - Cache file does not exist.
#==============================================================================
cache__get__secondary_workspace() {
  cat "$CACHE_FILE__SECONDARY_WORKSPACE"
}

#==============================================================================
# Get the value from the active scratchpad cache file if exists.
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
#   Value from the cache file.
# STDERR:
#   None
# Status:
#   0 - Cache file is present and value returned.
#   1 - Cache file does not exist.
#==============================================================================
cache__get__active_scratchpad() {
  cat "$CACHE_FILE__ACTIVE_SCRATCHPAD"
}

#==============================================================================
# SWAY ADAPTER
#==============================================================================

#==============================================================================
# Queries the primary workspace name.
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
#   Primary workspace name.
# STDERR:
#   None
# Status:
#   0 - Other status is not expected.
#==============================================================================
adapter__get__primary_workspace_name() {
  swaymsg --type get_workspaces | \
  jq --raw-output '..|try select(.focused == true) | .name'
}

#==============================================================================
# Queries the secondary workspace name.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] scratchpad_name - Name of the toggleable scratchpad name.
# STDIN:
#   None
#------------------------------------------------------------------------------
# Output variables:
#   None
# STDOUT:
#   Secondary workspace name.
# STDERR:
#   None
# Status:
#   0 - Scratchpad exists so secondary workspace can be returned.
#   1 - Scratchpad doesn't exist yet, secondary workspace cannot be returned.
#==============================================================================
adapter__get__secondary_workspace_name() {
  scratchpad_name="$1"

  scratchpad_output="$( \
    swaymsg --type get_workspaces | \
    jq --raw-output "..|try select(.name == \"${scratchpad_name}\") | .output" \
  )"

  if [ -n "$scratchpad_output" ]
  then
    swaymsg --type get_outputs | \
    jq --raw-output "..|try select(.name == \"${scratchpad_output}\") | .current_workspace"
  else
    return 1
  fi
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
adapter__switch_to_workspace() {
  workspace_name="$1"
  swaymsg workspace "$workspace_name"
}

#==============================================================================
# TOGGLE HANDLERS
#==============================================================================

#==============================================================================
# Function that toggles the scratchpad on while executing all the necessary
# persistency tasks.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] scratchpad_name - Scratchpad name that should be toggled on.
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
toggle_scratchpad__on() {
  scratchpad_name="$1"

  log "TOGGLE ON  [${scratchpad_name}]"

  primary_workspace_name="$(adapter__get__primary_workspace_name)"

  # Saving the scratchpad name and the primary workspace name.
  cache__set__active_scratchpad "$scratchpad_name"
  cache__set__primary_workspace "$primary_workspace_name"

  # The secondary workspace id can only be saved if the toggleable scratchpad
  # workspace was already exists. In this case the secondary workspace will be
  # the one that was active before the toggle event was called on the output
  # the scratchpad workspace was created into.
  if secondary_workspace_name="$(adapter__get__secondary_workspace_name "$scratchpad_name")"
  then
    cache__set__secondary_workspace "$secondary_workspace_name"
  fi

  adapter__switch_to_workspace "$scratchpad_name"
}

#==============================================================================
# Function that toggles the scratchpad off based on the previously saved data.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] scratchpad_name - Scratchpad name that should be toggled off.
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
toggle_scratchpad__off() {
  scratchpad_name="$1"

  # If the active scratchpad name is different than the currently received
  # one, the script should switch to the new scratchpad instead of switching
  # back to the original workspace.
  if active_scratchpad_name="$(cache__get__active_scratchpad)"
  then
    if [ "$scratchpad_name" != "$active_scratchpad_name" ]
    then
      replace_scratchpad "$scratchpad_name"
      return
    fi
  fi

  log "TOGGLE OFF [${scratchpad_name}]"

  # STEP 1 - Restore secondary workplace if needed.
  if old_secondary_workspace_name="$(cache__get__secondary_workspace)"
  then
    adapter__switch_to_workspace "$old_secondary_workspace_name"
  fi

  # STEP 2 - Restore primary workspace.
  primary_workspace_name="$(cache__get__primary_workspace)"
  adapter__switch_to_workspace "$primary_workspace_name"

  # STEP 3 - Clearing up the cache for the next toggle cycle.
  cache__invalidate
}

#==============================================================================
# Function that raplaces an existing scratchpad with a new one.
#------------------------------------------------------------------------------
# Globals:
#   None
# Arguments:
#   [1] new_scratchpad_name - Scratchpad name that should replace the existing
#       one.
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
replace_scratchpad() {
  new_scratchpad_name="$1"

  log "REPLACE    [${active_scratchpad_name}]->[${new_scratchpad_name}]"

  # STEP 1 - Restore previous secondary workplace if needed.
  if old_secondary_workspace_name="$(cache__get__secondary_workspace)"
  then
    adapter__switch_to_workspace "$old_secondary_workspace_name"
  fi

  # STEP 2 - Save secondary workspace for the new scratchpad.
  if new_secondary_workspace_name="$(adapter__get__secondary_workspace_name "$new_scratchpad_name")"
  then
    cache__set__secondary_workspace "$new_secondary_workspace_name"
  fi

  # STEP 3 - Save new scratchpad name.
  cache__set__active_scratchpad "$new_scratchpad_name"

  # STEP 4 - Switch to the new scratchpad.
  adapter__switch_to_workspace "$new_scratchpad_name"
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
#   [1] scratchpad_name - Scratchpad name that should be toggled.
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
  scratchpad_name="$1"

  if cache__has_content
  then
    toggle_scratchpad__off "$scratchpad_name"
  else
    toggle_scratchpad__on "$scratchpad_name"
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
  cache__invalidate
}

#==============================================================================
# ENTRY POINT
#==============================================================================

cache__init

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
      ;;
    * )
      display_help
      ;;
  esac
done

# Default action is to print out the help.
display_help
