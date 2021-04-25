#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

source ../../utils/libdeploy.bash

info "You should install the Krohnkite kwin plugin first manually!"
info "Settings -> Window Management -> KWin Scripts -> Get New Scripts.."
read -p " ?? | Press any key if you have installed Krohnkite. "
