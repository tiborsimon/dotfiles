#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/common.bash

link_package moc \
             ./config/ ${HOME}/.moc
