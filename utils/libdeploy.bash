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
# Initializes the error log file in the repository root.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function init_error_log {
  clean_up_error_log
  touch "${DOTFILES_ERROR_LOG_PATH}"
}

#######################################
# Writes line to error log.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function write_to_error_log {
  echo ------------------------------ >> "${DOTFILES_ERROR_LOG_PATH}"
  echo $@ >> "${DOTFILES_ERROR_LOG_PATH}"
}

#######################################
# Deletes the error log file.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function clean_up_error_log {
  rm -f "${DOTFILES_ERROR_LOG_PATH}"
}

#######################################
# Runs the given command and redirects it's ouput to the error log file. If the
# execution fails, the script exits.
# Globals:
#   None
# Arguments:
#   command - executable command
# Returns:
#   None
#######################################
function execute {
  write_to_error_log $@
  if ! $@&>>${DOTFILES_ERROR_LOG_PATH}; then
    fail "Last command failed. Check the error log at: ${BOLD}${RED}${DOTFILES_ERROR_LOG_PATH}${RESET}."
    exit 1
  fi
}

#######################################
# Installs the given packages with the predefined install command.
# Globals:
#   None
# Arguments:
#   package - installable package name
#   [..] - repeated intsallable package list
# Returns:
#   None
#######################################
function install_packages {
  info "Installing packages: $@"
  for package in $@; do
    if ! pacman -Qi $package &>/dev/null; then
      execute execute_with_privilege pacman -S --noconfirm --needed $package
    fi
  done
}

#######################################
# Installs the given AUR packages with the predefined install command.
# Globals:
#   None
# Arguments:
#   package - installable package name
#   [..] - repeated intsallable package list
# Returns:
#   None
#######################################
function install_aur_packages {
  using yay
  info "Installing AUR packages: $@"
  for package in $@; do
    if ! yay -Qi $package &>/dev/null; then
      execute yay -S --noconfirm --cleanafter $package
    fi
  done
}


#######################################
# Links the given target and link name pairs for the given package name,
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

    info "Linking ${display_target} to ${link_name}.."

    output=$(link_file $target $link_name)
    result=$?
    display_action $output

    while [ $result -ne 0 ]; do

      while [ $action != 'o' ] && [ $action != 'b' ] && [ $action != 's' ]; do
        warning "Target link ${BOLD}${link_name}${RESET} already exists!"
        user "What do you want to do? [${BOLD}${RED}o${RESET}] overwrite, [${BOLD}${BLUE}b${RESET}] backup, [${BOLD}${YELLOW}s${RESET}] skip? "

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

#######################################
# Helper function to be called before you call a third party tool that might
# not be installed on your system. This script will attempt to install it as
# the given tool installation method was previously implemented in the script.
# Globals:
#   None
# Arguments:
#   tool - 3rd party tool you want to use as a consequent command
# Returns:
#   0 - if the tool is already installed or just installed
#   1 - if the tool installation method was not found
#######################################
function using {
  # Getting the installable tool's name.
  tool=$1

  # Temporary path to download the stuff.
  TEMP_PATH='/tmp/dotfiles-sandbox'

  #######################################
  # YAY - the pacman like AUR installer
  #######################################
  if [ "$tool" = 'yay' ]; then
    if which yay&>/dev/null; then return 0; fi

    info "Installing yay.."

    # Creating a temporary location
    rm -rf ${TEMP_PATH}
    mkdir -p ${TEMP_PATH}
    pushd ${TEMP_PATH} &> /dev/null

    function get_yay_package {
      curl -sL http://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=yay-bin > PKGBUILD
      makepkg
      execute_with_privilege pacman --noconfirm -U $(ls | grep "yay.*xz")
    }

    execute get_yay_package

    # package is ready, we want to install it now
    # the package is az xz package
    # run_with_privilege pacman --noconfirm -U $(ls | grep "yay.*xz")

    popd &> /dev/null
    rm -rf ${TEMP_PATH}

    return 0
  fi

  #######################################
  # PIP - the python package manager
  #######################################
  if [ "$tool" = 'pip' ]; then
    if which pip&>/dev/null; then return 0; fi

    info "Installing pip.."
    execute execute_with_privilege pacman -S --noconfirm python-pip

    return 0
  fi

  #######################################
  # PIPSI - pip script installer
  #######################################
  if [ "$tool" = 'pipsi' ]; then
    if which pipsi&>/dev/null; then return 0; fi

    function install_pipsi {
      curl -sL https://raw.githubusercontent.com/mitsuhiko/pipsi/master/get-pipsi.py | python - --no-modify-path
    }

    info "Installing pipsi.."
    execute install_pipsi

    return 0
  fi

  #######################################
  # JQ - command line json processor
  #######################################
  if [ "$tool" = 'jq' ]; then
    if which jq&>/dev/null; then return 0; fi

    info "Installing jq.."
    execute execute_with_privilege pacman -S --noconfirm jq

    return 0
  fi

  echo "ERROR [using]: tool '${tool}' installation was not implemented!"
  return 1
}


RED=$(tput setaf 1)
RED_BG=$(tput setab 1)
GREEN=$(tput setaf 2)
YELLOW=$(tput setaf 3)
BLUE=$(tput setaf 4)
MAGENTA=$(tput setaf 5)
CYAN=$(tput setaf 6)
RESET=$(tput sgr0)
BOLD=$(tput bold)

function task {
  echo " ${BOLD}${BLUE}>>${RESET} | $@"
}

function info {
  echo " ${BOLD}${CYAN}..${RESET} | $@"
}

function user {
  echo " ${BOLD}${BLUE}??${RESET} | $@"
}

function success {
  echo " ${BOLD}${GREEN}ok${RESET} | $@"
}

function warning {
  echo " ${BOLD}${YELLOW}!!${RESET} | $@"
}

function fail {
  echo " ${BOLD}${RED}!!${RESET} | $@"
}


#==============================================================================
#  I N T E R N A L   H E L P E R   F U N C T I O N S
#==============================================================================
#  Do not call theese directly form your scripts!
#==============================================================================

#######################################
# Helper function to execute command with elevated privilege.
# Globals:
#   None
# Arguments:
#   command - command that is to be execute with elevated privileges
# Returns:
#   None
#######################################
function execute_with_privilege {
  temp_command=$(echo $@)
  sudo --reset-timestamp --preserve-env --shell --prompt="${BOLD}${YELLOW} !! ${RESET}| About to run privileged command: ${YELLOW}${temp_command}${RESET}. [sudo] password for ${BOLD}${USER}${RESET}: " $@
}


#######################################
# Helper function to display the executed actions.
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
        info "Old link deleted.";;
      moved )
        info "Moved to ${BLUE}${link_name}.backup${RESET}";;
      skipped )
        success "Skipped.";;
      exists )
        # success "Link exists with same target. Nothing to do.";;
        ;;
      linked )
        # success "Linked.";;
        ;;
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
        # link exist with the same target: nothing to do, return
        skip=true;
        echo exists
        return 0
      else
        # link exist with different target + no policy flag: return with error
        return 1
      fi
    fi

    if [ "$overwrite" == "true" ]; then
      if ! rm -rf "$link_name" &>/dev/null; then
        execute_with_privilege rm -rf "$link_name"
      fi
      echo deleted
    fi

    if [ "$backup" == "true" ]; then
      if ! mv "$link_name" "${link_name}.backup"; then
        execute_with_privilege mv "$link_name" "${link_name}.backup"
      fi
      echo moved
    fi
  fi

  if [ "$skip" == "false" ]; then

    if ! mkdir -p $(dirname "$link_name") &>/dev/null; then
      execute_with_privilege mkdir -p $(dirname "$link_name")
    fi

    # test if the link can be created
    if touch "$link_name" &>/dev/null; then
      rm $link_name
      ln -s "$target" "$link_name"
    else
      # propably there is no permission to create the link, so use sudo..
      execute_with_privilege ln -s "$target" "$link_name"
    fi

    echo linked
  else
    echo skipped
  fi
}

