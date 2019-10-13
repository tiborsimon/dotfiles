#######################################
# Configuration deployment functions.
#######################################

# Absolute root of the repository.
REPO_ROOT=$(git rev-parse --show-toplevel)

# A prefix can be specified that will be prepended to the link names to easily
# be able to distinguish them from the system commands. This also helps to
# search between them.
PREFIX="my"

# The preferred location is the user's local directory. Make sure that your
# PATH contains this location.
LINK_PATH="${HOME}/.local/bin"

# The fifo that connects the two panes of the installer window. It is created
# and deleted by the installer script.
LOG_PIPE="${REPO_ROOT}/dotfiles.fifo"

# Persistent error log file that will hold the last execution full log.
ERROR_LOG_FILE="${REPO_ROOT}/error.log"

# Config packages can store messages that will be displayed at the end of the
# installer script to remind the user various additional tasks.
MESSAGES_FILE="${REPO_ROOT}/messages.tmp"


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
  touch "${ERROR_LOG_FILE}"
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
  echo ------------------------------ >> "${ERROR_LOG_FILE}"
  echo "$@" >> "${ERROR_LOG_FILE}"
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
  rm -f "${ERROR_LOG_FILE}"
}

#######################################
# Initializes the messages file in the repository root.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function init_messages {
  clean_up_messages
  touch "${MESSAGES_FILE}"
}

#######################################
# Writes line to the messages file.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function write_to_messages {
  echo "$@" >> "${MESSAGES_FILE}"
}

#######################################
# Prints out the messages file content.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function display_messages {
  if [ -s ${MESSAGES_FILE} ]
  then
    echo ""
    echo "============================================================================="
    cat ${MESSAGES_FILE}
    echo "============================================================================="
  fi
}

#######################################
# Deletes the messages file.
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
#######################################
function clean_up_messages {
  rm -f "${MESSAGES_FILE}"
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
  if ! $@ 2>&1 | tee ${ERROR_LOG_FILE} ${LOG_PIPE}; then
    fail "Last command failed. Check the error log at: ${BOLD}${RED}${ERROR_LOG_FILE}${RESET}."
    exit 1
  fi
}

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
  read -n 1 -sp "${BOLD}${YELLOW} !! ${RESET}| About to run command with priviledge: '${YELLOW}${temp_command}${RESET}'. Do you want to continue? [y/N] " decision
  echo ''
  if [ "y" == "$decision" ]
  then
    write_to_error_log $@
    sudo --preserve-env --shell --prompt="${BOLD}${YELLOW} !! ${RESET}| [sudo] password for ${BOLD}${USER}${RESET}: " $@ 2>&1 | tee ${ERROR_LOG_FILE}
  else
    echo "${BOLD}${YELLOW} !! ${RESET}| Aborting.."
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
      execute_with_privilege pacman -S --noconfirm --needed $package
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
#   target - linkable target file
#   link_name - link
#   [..] - repeated target and link_name pairs
# Returns:
#   None
#######################################
function link_package {
  while [[ $# -gt 0 ]]; do
    local target=$(readlink -f $1)
    local display_target=$(basename $target)
    local link_name=$2
    local action='invalid'
    shift; shift

    local output=
    local result=

    info "Linking ${display_target} to ${link_name}.."

    output=$(link_file $target $link_name)
    result=$?
    display_action $output

    while [ $result -ne 0 ]; do

      while [ $action != 'o' ] && [ $action != 'b' ] && [ $action != 's' ]; do
        warning "Target link ${BOLD}${link_name}${RESET} already exists!"
        user "What do you want to do? [${BOLD}${RED}o${RESET}] overwrite, [${BOLD}${BLUE}b${RESET}] backup, [${BOLD}${YELLOW}s${RESET}] skip? "
        read -n 1 -sp '' action
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
# Returns:
#   None
#######################################
function link_script {
  local script_category=$1
  local script_path=$2

  # Making sure that the target path exists..
  mkdir -p ${LINK_PATH}

  # Getting the name without the extension.
  script_name=$(basename $script_path | cut -d. -f1)

  # Assembling the script's full name that would be linked to.
  script_full_name="${PREFIX}-${script_category}-${script_name}"

  # Calling the library linker fuction.
  link_package ${script_path} ${LINK_PATH}/${script_full_name}
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
      git clone https://aur.archlinux.org/yay.git
      cd yay
      makepkg -si
      cd ..
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
    execute_with_privilege pacman -S --noconfirm python-pip

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
    execute_with_privilege pacman -S --noconfirm jq

    return 0
  fi

  echo "ERROR [using]: tool '${tool}' installation was not implemented!"
  return 1
}

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

#######################################
# Function that prints a horizontal line to the sidebar. It can draw three type
# of lines.
# Globals:
#   SIDEBAR_WIDTH
#   LINE_COLOR
# Arguments:
#   None
# Flags:
#   --doube - double line
#   --top   - line that can be used as a sidebar top
# Returns:
#   None
#######################################
function line {
  local flag=${1:-}
  echo -n "${LINE_COLOR}"
  if [ "$flag" = "--double" ]
  then
    python -c "print('═' * 4 + '╪' + '═' * ($SIDEBAR_WIDTH - 5))"
  elif [ "$flag" = "--top" ]
  then
    python -c "print('─' * 4 + '┬' + '─' * ($SIDEBAR_WIDTH - 5))"
  else
    python -c "print('─' * 4 + '┼' + '─' * ($SIDEBAR_WIDTH - 5))"
  fi
  echo -n "${RESET}"
}

function fold_message {
  echo "$@" | fold --spaces --width=$(($SIDEBAR_WIDTH - 7)) | sed -e ":a;N;\$!ba;s/\n/\n    ${LINE_COLOR}│${RESET} /g"
}

function info {
  message=$(fold_message "$@")
  echo " ${BOLD}${CYAN}..${RESET} ${LINE_COLOR}│${RESET} $message"
}

function task {
  message=$(fold_message "$@")
  echo " ${BOLD}${BLUE}>>${RESET} ${LINE_COLOR}│${RESET} $message"
}

function success {
  message=$(fold_message "$@")
  echo " ${BOLD}${GREEN}ok${RESET} ${LINE_COLOR}│${RESET} $message"
}

function warning {
  message=$(fold_message "$@")
  echo " ${BOLD}${YELLOW}!!${RESET} ${LINE_COLOR}│${RESET} $message"
}

function fail {
  message=$(fold_message "$@")
  echo " ${BOLD}${RED}!!${RESET} ${LINE_COLOR}│${RESET} $message"
}

function render_option {
  local color=$1
  shift
  local key=$1
  shift
  local description=$1
  echo  "    ${LINE_COLOR}│${RESET}  ${BOLD}[${color}${key}${RESET}] $description"
}

function option {
  local cr=$(echo $'\n.')
  cr=${cr%.}

  local options=""
  local keys=""

  while [[ $# -gt 0 ]]
  do
    key="$1"
    case $key in
      -g|--green)
        options="${options}${cr}$(render_option ${GREEN} "$2" "$3")"
        keys="$keys $2"
        shift # past color
        shift # past key
        shift # past description
        ;;
      -y|--yellow)
        options="${options}${cr}$(render_option ${YELLOW} "$2" "$3")"
        keys="$keys $2"
        shift # past color
        shift # past key
        shift # past description
        ;;
      -b|--blue)
        options="${options}${cr}$(render_option ${BLUE} "$2" "$3")"
        keys="$keys $2"
        shift # past color
        shift # past key
        shift # past description
        ;;
      -r|--red)
        options="${options}${cr}$(render_option ${RED} "$2" "$3")"
        keys="$keys $2"
        shift # past color
        shift # past key
        shift # past description
        ;;
      -m|--magenta)
        options="${options}${cr}$(render_option ${MAGENTA} "$2" "$3")"
        keys="$keys $2"
        shift # past color
        shift # past key
        shift # past description
        ;;
      -c|--cyan)
        options="${options}${cr}$(render_option ${CYAN} "$2" "$3")"
        keys="$keys $2"
        shift # past color
        shift # past key
        shift # past description
        ;;
      *)
        local prompt="$@"
        shift
        ;;
    esac
  done

  line
  prompt=$(fold_message "$prompt")

  if [ -n "$keys" ]
  then
    result="│"
    while ! echo "$keys" | grep --silent "$result"
    do
      echo " ${BOLD}${YELLOW}??${RESET} ${LINE_COLOR}│${RESET} ${prompt}"
      echo "    ${LINE_COLOR}│${RESET}${options}"
      echo "    ${LINE_COLOR}│${RESET}"
      echo -n "    ${LINE_COLOR}│${RESET} > "
      read -n 1 -p "" result
      echo ""
      line
    done
  else
    echo " ${BOLD}${YELLOW}??${RESET} ${LINE_COLOR}│${RESET} ${prompt}"
    echo -n "    ${LINE_COLOR}│${RESET} > "
    read -n 1 -p "" result
    echo ""
    line
  fi
}



#==============================================================================
#  I N T E R N A L   H E L P E R   F U N C T I O N S
#==============================================================================
#  Do not call theese directly form your scripts!
#==============================================================================


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
