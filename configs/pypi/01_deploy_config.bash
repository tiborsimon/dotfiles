#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package pypi \
             ./config/pypirc ${HOME}/.pypirc
