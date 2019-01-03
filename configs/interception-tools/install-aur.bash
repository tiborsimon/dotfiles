#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

../../utils/lib/using.bash yay

yay -S interception-tools interception-caps2esc
