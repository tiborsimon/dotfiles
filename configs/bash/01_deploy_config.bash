#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package bash \
             ./config/bashrc.bash ${HOME}/.bashrc \
             ./config/bash_profile.bash ${HOME}/.bash_profile \
             ./scripts/git-completion.bash ${HOME}/.scripts/git-completion.bash \
             ./scripts/gitstatus.bash ${HOME}/.scripts/gitstatus.bash \
             ./scripts/gitstatus.py ${HOME}/.scripts/gitstatus.py \
             ./scripts/define-colors.bash ${HOME}/.scripts/define-colors.bash
