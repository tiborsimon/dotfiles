#!/usr/bin/env bash
#######################################
# Custom script installation script. These scripts cannot be added to a
# concrete configuration therefore there are put together in categories in this
# separate script section.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
# Sane environment
set -o errexit -o pipefail -o noclobber -o nounset
# Switching to the repository root
cd $(git rev-parse --show-toplevel)

source ./utils/dotfiles.lib.bash


#==============================================================================
# CONFIGURATION

VERSION="v2.0.0"
SIDEBAR_WIDTH=42
LINE_COLOR="$GREEN"
TMUX_LINE_COLOR="green"


#==============================================================================
# GLOBAL VARIABLES

FIFO="dotfiles.fifo"

TMUX_SOCKET="dotfiles_socket"
TMUX_SESSION="dotfiles_session"

ROLE_FRAME="frame"
ROLE_SIDEBAR="sidebar"
ROLE_LOGS="logs"
ROLE_LOGO="logo"

TARGET_ALL="__all"


#==============================================================================
# PARAMETER PARSING

ROLE=$ROLE_FRAME
TARGET=$TARGET_ALL

while [[ $# -gt 0 ]]
do
  key="$1"
  case $key in
    -r|--role)
      ROLE="$2"
      shift # past argument
      shift # past value
      ;;
    -t|--target)
      TARGET="$2"
      shift # past argument
      shift # past value
      ;;
    *)
      ;;
  esac
done


#==============================================================================
# FRAME ROLE

function at_exit {
  rm -f $FIFO
  tmux -L $TMUX_SOCKET kill-session 2>/dev/null
}

function frame_main {
  rm -f $FIFO
  mkfifo $FIFO
  local width=$(tput cols)
  local height=$(tput lines)

  # Initializing new plain tmux session
  tmux -L $TMUX_SOCKET -f /dev/null new-session -d -x $width -y $height -s $TMUX_SESSION "/bin/bash --noprofile --norc"
  tmux -L $TMUX_SOCKET set status off
  tmux -L $TMUX_SOCKET set mouse on
  tmux -L $TMUX_SOCKET set pane-border-style fg=${TMUX_LINE_COLOR}
  tmux -L $TMUX_SOCKET set pane-active-border-style fg=${TMUX_LINE_COLOR}

  tmux -L $TMUX_SOCKET split-window -h "/bin/bash --noprofile --norc"

  tmux -L $TMUX_SOCKET select-pane -t 0
  tmux -L $TMUX_SOCKET send-keys "./utils/install.bash --role ${ROLE_LOGO}" C-m

  tmux -L $TMUX_SOCKET split-window -v "/bin/bash --noprofile --norc"
  tmux -L $TMUX_SOCKET resize-pane -y 7 -t 0
  tmux -L $TMUX_SOCKET resize-pane -x $SIDEBAR_WIDTH -t 0

  tmux -L $TMUX_SOCKET select-pane -t 1
  tmux -L $TMUX_SOCKET send-keys "./utils/install.bash --role ${ROLE_SIDEBAR}" C-m

  tmux -L $TMUX_SOCKET select-pane -t 2
  tmux -L $TMUX_SOCKET send-keys "./utils/install.bash --role ${ROLE_LOGS}" C-m
  tmux -L $TMUX_SOCKET select-pane -t 1
  tmux -L $TMUX_SOCKET attach
}


#==============================================================================
# PROGRESS ROLE

function sidebar_main {
  clear

  echo ""
  echo "     ${BOLD}Modular Dotfiles System ${VERSION}${RESET}"
  echo ""

  line --top

  task "Looking for dotfiles modules.."
  line
  info "This is some long line that is a bit too long for the width of the column.."
  option \
    --green g "This is a green option" \
    --blue b "This is a blue option" \
    --red r "This is a red option" \
    --yellow y "This is a yellow option" \
    --magenta m "This is a magenta option" \
    --cyan c "This is a cyan option" \
    "Press any button to exit.. And this is a bit longer version of this command.."
  info "You have selected the '$result' option."
  option "Press any button to exit.."
  read
}


#==============================================================================
# LOGS ROLE

function logs_main {
  clear
  echo "> Log page started. Waiting for logs.."
  echo ""
  cat <>${FIFO}
}


#==============================================================================
# LOGO ROLE

function logo_main {
  clear
  echo -n "${GREEN}"
  echo '        _       _    __ _ _'
  echo '       | |     | |  / _(_) |'
  echo '     __| | ___ | |_| |_ _| | ___  ___'
  echo '    / _` |/ _ \| __|  _| | |/ _ \/ __|'
  echo '   | (_| | (_) | |_| | | | |  __/\__ \'
  echo '  (_)__,_|\___/ \__|_| |_|_|\___||___/'
  echo -n "${RESET}"
  read -n 1 -s
}


#==============================================================================
# MAIN ROLE SWITCH AND ENTRY POINTS

trap at_exit EXIT

case $ROLE in
  ${ROLE_FRAME})
    frame_main
    ;;
  ${ROLE_SIDEBAR})
    sidebar_main
    ;;
  ${ROLE_LOGS})
    logs_main
    ;;
  ${ROLE_LOGO})
    logo_main
    ;;
  *)
    echo "Invalid parameter.."
    exit 1
    ;;
esac

