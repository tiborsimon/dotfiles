#!/usr/bin/env bash
source $HOME/.dotfiles/functions/commons
read branch remote staged conflicts changed untracked stashed clean <<<$(python $HOME/.dotfiles/bash/gitstatus.py)

if [[ -z $branch ]]; then 
  exit 0
else
  if [ "$clean" == 1 ]; then
    echo -en "${BoldGreen}"
  else
    echo -en "${BoldRed}"
  fi
  echo -en "$branch${ResetColor}"
fi

if [ "$remote" != "." ]; then
  echo -en " ${BoldYellow}${remote}${ResetColor}"
fi

if [ "$staged" != "0" ]; then
  echo -en " $staged staged"
fi

if [ "$changed" != "0" ]; then
  echo -en " $changed changed"
fi

if [ "$untracked" != "0" ]; then
  echo -en " $untracked untracked"
fi

if [ "$conflicts" != "0" ]; then
  echo -en " $conflicts conflicts"
fi

if [ "$stashed" != "0" ]; then
  echo -en " $stashed stashed"
fi
