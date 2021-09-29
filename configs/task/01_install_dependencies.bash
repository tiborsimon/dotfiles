#!/usr/bin/env bash
cd "$(dirname "$(readlink -f "$0")")" || exit

source ../../utils/libdeploy.bash

install_packages task timew

using pip
execute pip install --user tasklib

mkdir -p "${HOME}/.task"
cp '/usr/share/doc/timew/ext/on-modify.timewarrior' "${HOME}/.task/hooks/"
chmod +x "${HOME}/.task/hooks/on-modify.timewarrior"
