#######################################
# Configuration deployment functions.
#######################################

# A prefix can be specified that will be prepended to the link names to easily
# be able to distinguish them from the system commands. This also helps to
# search between them.
PREFIX="my"

# The preferred location is the user's local directory. Make sure that your
# PATH contains this location.
LINK_PATH="${HOME}/.local/bin"

#==============================================================================
#  A P I   F U N C T I O N S
#==============================================================================

#######################################
# Installs the given target and link name pairs for the given package name,
# while it takes care of the issues with the existing files.
# Globals:
#   None
# Arguments:
#   package_name - name of the package
#   target - linkable target file
#   link_name - link
#   [..] - repeated target and link_name pairs
# Returns:
#   None
#######################################
function link_package {
  local package_name=$1
  shift

  while [[ $# -gt 0 ]]; do
    local target=$(readlink -f $1)
    local display_target=$(basename $target)
    local link_name=$2
    local action='invalid'
    shift; shift

    local output=, result=

    task $package_name "Linking ${YELLOW}${display_target}${RESET} to ${CYAN}${link_name}${RESET}"

    output=$(link_file $target $link_name)
    result=$?
    display_action $output

    while [ $result -ne 0 ]; do

      while [ $action != 'o' ] && [ $action != 'b' ] && [ $action != 's' ]; do
        warning $package_name "Target link ${CYAN}${link_name}${RESET} already exists!"
        user $package_name "What do you want to do? [${BOLD}${RED}o${RESET}] overwrite, [${BOLD}${BLUE}b${RESET}] backup, [${BOLD}${YELLOW}s${RESET}] skip? "

        read -n 1 action
        echo ''
      done

      output=$(link_file $target $link_name $action)
      result=$?
      display_action $output

    done
  done
}

#######################################
# Installs the given scripts with a preconfigured prefix and location path. It
# expects a category name as the first parameter and a list of installable
# script paths. It constructs the final callable link name as follows:
#
# <PREFIX>-<CATEGORY>-<script name without extension>
#
# Globals:
#   None
# Arguments:
#   category_name - name of the scripts category
#   script_path - path of the linkable script
#   [..] - repeated script_paths
# Returns:
#   None
#######################################
function link_scripts {
  local script_category=$1
  shift

  # Making sure that the target path exists..
  mkdir -p ${LINK_PATH}

  while [[ $# -gt 0 ]]; do
    # Getting the next script path.
    local script_path=$1
    shift

    # Getting the name without the extension.
    script_name=$(basename $script_path | cut -d. -f1)

    # Assembling the script's full name that would be linked to.
    script_full_name="${PREFIX}-${script_category}-${script_name}"

    # Calling the library linker fuction.
    link_package scripts ${script_path} ${LINK_PATH}/${script_full_name}
  done
}



#==============================================================================
#  I N T E R N A L   H E L P E R   F U N C T I O N S
#==============================================================================
#  Do not call theese directly form your scripts!
#==============================================================================

RED=$(tput setaf 1)
RED_BG=$(tput setab 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
RESET=$(tput sgr0)
BOLD=$(tput bold)

PACKAGE_PRINT_WIDTH=10

function print_package {
  local package=$1
  echo "${BOLD}$(printf "%${PACKAGE_PRINT_WIDTH}s" $package)${RESET}"
}

function task {
  local package=$1
  shift
  printf "$(print_package $package) | ${BOLD}${BLUE}>>${RESET} | $@\n"
}

function info {
  local package=$1
  shift
  printf "$(print_package $package) | ${BOLD}${BLUE}..${RESET} | $@\n"
}

function user {
  local package=$1
  shift
  printf "$(print_package $package) | ${BOLD}${CYAN}??${RESET} | $@"
}

function success {
  local package=$1
  shift
  printf "$(print_package $package) | ${BOLD}${GREEN}OK${RESET} | $@\n"
}

function warning {
  local package=$1
  shift
  printf "$(print_package $package) | ${BOLD}${YELLOW}!!${RESET} | $@\n"
}

function fail {
  local package=$1
  shift
  printf "${BOLD}${package}${RESET} |${BOLD}${RED}FAIL${RESET}| $@\n"
}

#######################################
# Helper function do display the executed actions.
# Globals:
#   None
# Arguments:
#   output - raw ouput of the link_file function
# Returns:
#   None
#######################################
function display_action {
  for out in $@; do
    case "$out" in
      deleted )
        info $package_name "Old link deleted.";;
      moved )
        info $package_name "Moved to ${BLUE}${link_name}.backup${RESET}";;
      skipped )
        success $package_name "Skipped.";;
      linked )
        success $package_name "Linked.";;
      * )
        ;;
    esac
  done
}


#######################################
# Create a symbolic link to a given target file. If the link already exists
# with the same target, the link creation will be skipped as there is nothing
# to do. If the link already exists but with a different target, the function
# will return, stating the issue with the return value. You can provide a link
# handling policy flag, that will determine the behavior in the consequent
# function call. If the policy flag is already set on the first call, the
# function will act immediately according to the flag. Beside the the return
# value, it outputs the command it executes.
# Globals:
#   None
# Arguments:
#   target - linkable target file
#   link_name - link
#   policy - link handling policy flag
#     ''  - not specified, will return if exists
#     'o' - overwrite if exists
#     'b' - backup if exists
#     's' - skip if exists
# Returns:
#   0 - target linked or skipped
#   1 - link already exists with other target
# Outputs:
#   'deleted' - the old link deleted before overwrite
#   'moved' - the old link moved to a backup file (*.backup)
#   'skipped' - the linking was skipped
#   'linked' - target was linked
#######################################
function link_file {
  local target=$1
  local link_name=$2
  local policy_flag=$3

  local overwrite=false
  local backup=false
  local skip=false
  local no_policy=false

  case "$policy_flag" in
    o )
      overwrite=true;;
    b )
      backup=true;;
    s )
      skip=true;;
    * )
      no_policy=true;;
  esac

  # checking if the link already exists
  if [ -f "$link_name" -o -d "$link_name" -o -L "$link_name" ]; then

    if [ "$no_policy" == "true" ]; then

      local current_target="$(readlink $link_name)"

      if [ "$current_target" == "$target" ]; then
        # link exist with the same target: nothing to do
        skip=true;
      else
        # link exist with different target + no policy flag: return with error
        exit 1
      fi
    fi

    if [ "$overwrite" == "true" ]; then
      if ! rm -rf "$link_name" &>/dev/null; then
        sudo -k --prompt="           |${BOLD}${RED_BG} !! ${RESET}| Permission denied! [sudo] password for user $USER: " rm -rf "$link_name"
      fi
      echo deleted
    fi

    if [ "$backup" == "true" ]; then
      if ! mv "$link_name" "${link_name}.backup"; then
        sudo -k --prompt="           |${BOLD}${RED_BG} !! ${RESET}| Permission denied! [sudo] password for user $USER: " mv "$link_name" "${link_name}.backup"
      fi
      echo moved
    fi
  fi

  if [ "$skip" == "false" ]; then

    # test if the link can be created
    if touch "$link_name" &>/dev/null; then
      rm $link_name
      ln -s "$target" "$link_name"
    else
      # propably there is no permission to create the link, so use sudo..
      sudo -k --prompt="           |${BOLD}${RED_BG} !! ${RESET}| Permission denied! [sudo] password for user $USER: " ln -s "$target" "$link_name"
    fi

    echo linked
  else
    echo skipped
  fi
}

