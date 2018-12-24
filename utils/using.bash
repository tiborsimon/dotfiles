#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

tool=$1

TEMP_PATH='/tmp/dotfiles-sandbox'

if [ "$tool" = 'yay' ]; then
  echo "Installing yay.."

  mkdir -p ${TEMP_PATH}
  pushd ${TEMP_PATH} &> /dev/null

  git clone https://aur.archlinux.org/yay.git
  cd yay
  makepkg -si

  popd &> /dev/null
  rm -rf ${TEMP_PATH}

  echo "yay installed."
fi

