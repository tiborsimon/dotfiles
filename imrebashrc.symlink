# Setting PATH for Python 3.4
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# ==============================================================================
#  A L I A S E S
#

alias site='cd ~/Repos/tiborsimon.io && source site-env/bin/activate && clear'
alias buspirate='screen /dev/tty.usbserial-AD01W63E 115200 8N1'
alias github='cd ~/github'

alias appserver='ssh tibor@46.101.202.81'
alias proxyserver='ssh tibor@46.101.115.184'

alias ls='ls -lahpTG'

alias gs='git status'
alias ga='git add --all'
alias ..='cd ..'

function commitFunction {
  git commit -m $1
}
alias gc=commitFunction

function cloneFunction {
  cd ~/github
  git clone $1
  cd $(python -c "import re, sys; m=re.search('([-\w]+)\.git', sys.argv[1]); print(m.group(1))" $1)
}
alias gclone=cloneFunction



# ==============================================================================
#  G I T   P R O M P T
#  Source: https://coderwall.com/p/pn8f0g/show-your-git-status-and-branch-in-color-at-the-command-prompt

function git_branch {
  local COLOR_RED="\033[1;31m"
  local COLOR_YELLOW="\033[0;33m"
  local COLOR_GREEN="\033[0;32m"
  local COLOR_WHITE="\033[0;37m"
  local COLOR_RESET="\033[0m"

  local git_status="$(git status 2> /dev/null)"

  if [[ $git_status =~ "fatal: Not a git repository" ]]; then
    echo -en $COLOR_WHITE
  elif [[ $git_status =~ "Your branch is ahead of" ]]; then
    echo -en $COLOR_YELLOW
  elif [[ $git_status =~ "nothing to commit" ]]; then
    echo -en $COLOR_GREEN
  else
    echo -en $COLOR_RED
  fi

  local on_branch="On branch ([^${IFS}]*)"
  local on_commit="HEAD detached at ([^${IFS}]*)"

  if [[ $git_status =~ $on_branch ]]; then
    local branch=${BASH_REMATCH[1]}
    echo -n "($branch)"
  elif [[ $git_status =~ $on_commit ]]; then
    local commit=${BASH_REMATCH[1]}
    echo -n "($commit)"
  fi

  echo -en $COLOR_RESET
}

PS1="\n[\u] @ [\w] \$(git_branch)"
PS1+="\n [\#] → "
export PS1

function prompt {
  export PS1="\n[\u] @ \w \$(git branch 2> /dev/null)\n [\#] → "
  export PS2="|→"
}

export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
