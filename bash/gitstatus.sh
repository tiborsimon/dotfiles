#!/usr/bin/env bash
source $HOME/.dotfiles/functions/commons
read branch remote staged conflicts changed untracked stashed clean <<<$(python $HOME/.dotfiles/bash/gitstatus.py)

if [[ -z $branch ]]; then 
  exit 0
else
  #echo -en "["
  echo -en " on "
  if [ "$clean" == 1 ]; then
    echo -en "  ${BoldGreen}"
  else
    if [ "$changed" != "0" ]; then
      echo -en "${BoldRed}"
    else
      echo -en "${BoldMagenta}"
    fi
  fi
  echo -en "$branch${ResetColor}"
fi

if [ "$remote" != "." ]; then
  echo -en " ${BoldYellow}${remote}${ResetColor}"
fi

if [ "$staged" != "0" ]; then
  echo -en " ${DimGreen}$staged staged${ResetColor}"
fi

if [ "$changed" != "0" ]; then
  echo -en " ${DimRed}$changed changed${ResetColor}"
fi

if [ "$untracked" != "0" ]; then
  echo -en " ${DimMagenta}$untracked untracked${ResetColor}"
fi

if [ "$conflicts" != "0" ]; then
  echo -en " ${BoldRed}$conflicts conflicts${ResetColor}"
fi

if [ "$stashed" != "0" ]; then
  echo -en " ${DimBlue}$stashed stashed${ResetColor}"
fi
#echo -en "]"
