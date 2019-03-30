#    _               _
#   | |             | |
#   | |__   __ _ ___| |__  _ __ ___
#   | '_ \ / _` / __| '_ \| '__/ __|
#  _| |_) | (_| \__ \ | | | | | (__
# (_)_.__/ \__,_|___/_| |_|_|  \___|
#

source $HOME/.scripts/define-colors.bash
source $HOME/.scripts/git-completion.bash


stty -ixon # Disable ctrl-s and ctrl-q.
shopt -s autocd #Allows you to cd into directory merely by typing the directory name.
HISTSIZE= HISTFILESIZE= # Infinite history.

# Makefile autocomplete
complete -W "\`grep -oE '^[a-zA-Z0-9_-]+:([^=]|$)' Makefile | sed 's/[^a-zA-Z0-9_-]*$//'\`" make

# SSH handling
# http://rabexc.org/posts/pitfalls-of-ssh-agents
ssh-add -l &>/dev/null
if [ "$?" == 2 ]; then
  test -r ~/.ssh-agent && \
    eval "$(<~/.ssh-agent)" >/dev/null

  ssh-add -l &>/dev/null
  if [ "$?" == 2 ]; then
    (umask 066; ssh-agent > ~/.ssh-agent)
    eval "$(<~/.ssh-agent)" >/dev/null
    ssh-add
  fi
fi


# Handling location persistency between shell sessions
LAST_LOCATION_FILE="${HOME}/.last_location"
function cd {
  builtin cd $@
  pwd > $LAST_LOCATION_FILE
}

if [[ -a $LAST_LOCATION_FILE ]]
then
  builtin cd $(cat $LAST_LOCATION_FILE)
fi


# Some aliases
alias ..='cd ..'
alias dot='cd ~/.dotfiles'
alias org='cd ~/secrets/volume/org'
alias pro='cd ~/projects; ls -la'
alias sand='cd ~/sandbox; ls -la'
alias buspirate='sudo screen /dev/ttyUSB0 115200 8N1'
alias pubkey="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"
alias ls='ls --human-readable --color --group-directories-first'
alias gs='git status'
alias SS="sudo systemctl"
alias v="vim"
alias sv="sudo vim"
alias r="ranger"
alias sr="sudo ranger"
alias mkd="mkdir -pv"

function rg {
  # command rg -p $@ | less -RMFXK
  if [ -t 1 ]; then
      command rg --pretty "$@" \
          | less --RAW-CONTROL-CHARS --LONG-PROMPT --quit-if-one-screen --no-init --quit-on-intr
  else
      command rg "$@"
  fi
}

# Adding color
alias ls='ls -hN --color=auto --group-directories-first'
alias grep="grep --color=auto" # Color grep - highlight desired sequence.
alias ccat="highlight --out-format=ansi" # Color cat - print file with syntax highlighting.

# Internet
alias yd="youtube-dl --add-metadata -ic" # Download video link
alias yda="youtube-dl --extract-audio --audio-quality 0 --audio-format flac --ignore-errors --continue" # Download only audio
alias YT="youtube-viewer"
alias ethspeed="speedometer -r enp0s25"
alias wifispeed="speedometer -r wlp3s0"
alias starwars="telnet towel.blinkenlights.nl"


# Prompt handling
export PROMPT_COMMAND=__prompt_command  # Func to gen PS1 after CMDs

function set_virtualenv () {
  # Determine active Python virtualenv details.
  if test -z "$VIRTUAL_ENV" ; then
      PYTHON_VIRTUALENV=""
  else
      PYTHON_VIRTUALENV="${BoldBlue}[`basename \"$VIRTUAL_ENV\"`]${ResetColor} "
  fi
}

function __prompt_command() {
    local EXIT="$(printf "%03d" $?)"
    set_virtualenv

    PS1="\n${PYTHON_VIRTUALENV}\[${BoldWhite}\]\u\[${ResetColor}\] at \[${BoldWhite}\]\h\[${ResetColor}\] in \[${BoldWhite}\]\w\[${ResetColor}\]"

    if [[ ! -f .ignore-git-status ]]; then
        local git_status="$($HOME/.scripts/gitstatus.bash)"
        if [[ ! -z $git_status ]]; then
            PS1+="${git_status}\n"
        else
            PS1+="\n"
        fi
    else
        PS1+="git status ignored\n"
    fi

    PS1+="\t "

    if [ "$EXIT" != "000" ]; then
        PS1+="\[${BoldRed}\]"
    else
        PS1+="\[${BoldGreen}\]"
    fi

    PS1+="${EXIT}\[${ResetColor}\] "

    PS1+="\\$ \[$(tput sgr0)\]"
}

# Virtualenvwrapper config
export WORKON_HOME=~/.virtualenvs
source ~/.local/bin/virtualenvwrapper.sh

my-keyboard-reset >/dev/null

# Add API keys as environment variables
API_KEYS_FILE="${HOME}/.api_keys"
if [[ -a $API_KEYS_FILE ]]
then
  source $API_KEYS_FILE
fi
