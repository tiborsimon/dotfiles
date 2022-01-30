#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

link_package \
  './config/ledgerrc' "${HOME}/.config/ledger/ledgerrc"

link_scripts ledger \
  './scripts/accounts.bash' \
  './scripts/base.bash' \
  './scripts/formatter.py' \
  './scripts/shared-list.bash' \
  './scripts/summary.py' \
  './scripts/details.py' \
  './scripts/daily-summary.bash' \
  './scripts/weekly-summary.bash'
