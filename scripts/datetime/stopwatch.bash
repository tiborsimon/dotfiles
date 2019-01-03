#!/usr/bin/env bash

KEZDES=$(date +%s) watch --interval 0.2 --no-title 'for i in $(seq $(($(tput lines) / 2 - 3)) ); do echo ""; done; figlet -c -f lean -w $(tput cols) "$(python -c "import sys;s=int(sys.argv[1]);m,s=divmod(s,60);h,m=divmod(m,60);print(sys.argv[2].format(h,m,s))" $(($(date +%s) - $KEZDES)) "{:02} : {:02} : {:02}")"'
