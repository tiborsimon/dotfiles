#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/common.bash

link_package bash \
             ./config/bashrc ${HOME}/.bashrc \
             ${HOME}/.profile ${HOME}/.bash_profile \
             ./scripts/git-completion.bash ${HOME}/.scripts/git-completion.bash \
             ./scripts/gitstatus.bash ${HOME}/.scripts/gitstatus.bash \
             ./scripts/gitstatus.py ${HOME}/.scripts/gitstatus.py \
             ./scripts/define-colors.bash ${HOME}/.scripts/define-colors.bash
