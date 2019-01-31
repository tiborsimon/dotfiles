#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

link_package ledger \
             ./config/ledgerrc ${HOME}/.ledgerrc

link_scripts ledger \
             ./scripts/formatter.py