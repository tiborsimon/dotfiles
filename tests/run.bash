#!/usr/bin/env bash

set -e
cd $(dirname $(readlink -f $0))

function install_bats {
  if ! which bats&>/dev/null; then
    echo "Installing ${BOLD}bats${RESET}.."
    local ppp=tempdir
    rm -rf ./${ppp}
    git clone --depth 1 https://github.com/bats-core/bats-core.git ${ppp}
    cd ${ppp}
    ./install.sh ${HOME}/.local
    cd ..
    rm -rf ./${ppp}
  fi
}

install_bats
bats -r .
