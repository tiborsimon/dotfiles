#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package \
  ./config/gitconfig ${HOME}/.gitconfig

link_package \
  ./scripts/git-sha.bash ${HOME}/.local/bin/git-sha \
  ./scripts/git-checkown.bash ${HOME}/.local/bin/git-checkown \
  ./scripts/git-delouse.bash ${HOME}/.local/bin/git-delouse
