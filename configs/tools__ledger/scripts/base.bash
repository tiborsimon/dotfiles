#!/bin/sh

set -e

ledger --init-file ~/.config/ledger/ledgerrc $@
