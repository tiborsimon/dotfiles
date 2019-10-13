#!/usr/bin/env bash
set -o errexit
set -o nounset
# set -o pipefail


#==============================================================================
#  C O L O R E D   O U T P U T   M A N A G E M E N T
#==============================================================================
if which tput &>/dev/null
then
  RED=$(tput setaf 1)
  RED_BG=$(tput setab 1)
  GREEN=$(tput setaf 2)
  YELLOW=$(tput setaf 3)
  BLUE=$(tput setaf 4)
  MAGENTA=$(tput setaf 5)
  CYAN=$(tput setaf 6)
  RESET=$(tput sgr0)
  BOLD=$(tput bold)
else
  RED=""
  RED_BG=""
  GREEN=""
  YELLOW=""
  BLUE=""
  MAGENTA=""
  CYAN=""
  RESET=""
  BOLD=""
fi


#==============================================================================
#  H E L P E R   F U N C T I O N S
#==============================================================================

#######################################
# Returns the current branch or the current commit hash in case of a detached
# HEAD.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   Name of the current branch or the hash the HEAD is detached to.
#######################################
function get_branch {
  local status=$(git branch)
  if echo "$status" | grep --silent "detached"
  then
    echo $(git rev-parse --short HEAD)
    return 0
  fi

  if echo "$status" | grep --silent "rebasing"
  then
    echo $(echo "$status" | grep --perl-regex --only-matching "([^\s\)]+)\)$" | grep --perl-regex --only-matching "[^\s\)]+")
    return 0
  fi

  echo $(echo "$status" | grep --perl-regex "^\*" | cut --delimiter=' ' --fields=2)
}

#######################################
# Calculates and returns if the remote is ahead or behind the current local
# branch. It uses fancy arrows to display that status.
# Globals:
#   None
# Arguments:
#   Git status string produced by the `git status --porcelain --branch`
#   command.
# Returns:
#   State of the remote with fancy arrows if needed.
#######################################
function get_remote {
  local status=$(echo "$1" | head -n 1)

  ahead=$(echo $status | grep --perl-regex --only-matching "ahead \d+" | cut --delimiter=' ' --fields=2)
  behind=$(echo $status | grep --perl-regex --only-matching "behind \d+" | cut --delimiter=' ' --fields=2)

  if [ -n "$ahead" ]
  then
    ahead=$"↑$ahead"
  fi

  if [ -n "$behind" ]
  then
    behind=$"↓$behind"
  fi

  echo "${ahead}${behind}"
}

#==============================================================================
#  I N P U T   V A R I A B L E S
#==============================================================================
status=$(git status --porcelain --branch 2> /dev/null)
repo_root=$(git rev-parse --show-toplevel)


#==============================================================================
#  B R A N C H   A N D   R E M O T E   S T A T U S
#==============================================================================
branch=$(get_branch)
remote=$(get_remote "$status")


#==============================================================================
#  R E P O   S T A T U S
#==============================================================================
#  source: https://git-scm.com/docs/git-status#_short_format
staged=$(echo "$status"    | LC_ALL=C grep --perl-regex --only-matching "^(M[\sMD]|A[\sMD]|D[\s]|R[\sMD]|C[\sMD])" | wc --lines)
changed=$(echo "$status"   | LC_ALL=C grep --perl-regex --only-matching "^([\sMARC]M|[\sMARC]D|[\sD]R|[\sD]C)"     | wc --lines)
conflict=$(echo "$status"  | LC_ALL=C grep --perl-regex --only-matching "^(DD|UU|AA|AU|UA|DU|UD)"                  | wc --lines)
untracked=$(echo "$status" | LC_ALL=C grep --perl-regex --only-matching "^\?\?"                                    | wc --lines)


#==============================================================================
#  S T A S H   S T A T U S
#==============================================================================
stash_file="${repo_root}/.git/logs/refs/stash"
if [ -f $stash_file ]
then
  stashed=$(cat $stash_file | wc --lines)
else
  stashed=0
fi


#==============================================================================
#  F I N A L   R E P O   S T A T U S
#==============================================================================
if [ "${staged}${changed}${conflict}" -gt "0" ]
then
  clean=0
else
  clean=1
fi


#==============================================================================
#  D E B U G   P R I N T O U T
#==============================================================================
# echo ""
# echo "repo_root: '$repo_root'"
# echo "branch: '$branch'"
# echo "remote: '$remote'"
# echo "staged: '$staged'"
# echo "changed: '$changed'"
# echo "conflict: '$conflict'"
# echo "untracked: '$untracked'"
# echo "stashed: '$stashed'"
# echo "clean: '$clean'"
# exit 0


#==============================================================================
#  P R O D U C T I O N   P R I N T O U T
#==============================================================================
echo -en " on "
if [ "$clean" -eq "1" ]; then
  echo -en "${BOLD}${GREEN}"
else
  echo -en "${BOLD}${RED}"
fi
echo -en "$branch${RESET}"

if [ -n "$remote" ]; then
  echo -en " ${BOLD}${YELLOW}${remote}${RESET}"
fi

if [ "$staged" -gt "0" ]; then
  echo -en " ${GREEN}$staged staged${RESET}"
fi

if [ "$changed" -gt "0" ]; then
  echo -en " ${RED}$changed changed${RESET}"
fi

if [ "$untracked" -gt "0" ]; then
  echo -en " ${MAGENTA}$untracked untracked${RESET}"
fi

if [ "$conflict" -gt "0" ]; then
  echo -en " ${BOLD}${RED}$conflict conflicts${RESET}"
fi

if [ "$stashed" -gt "0" ]; then
  echo -en " ${BLUE}$stashed stashed${RESET}"
fi
