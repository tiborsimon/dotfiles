#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

OUTPUT=$1

WIFI_ID="wlp3s0"

WIFI_ICON="%{T3}直 %{T1}"
VPN_ICON="%{T1} %{T1}"

status=$(nmcli -t connection show --active)

function render_wifi {
  local status=$1
  if echo $status | grep --quiet $WIFI_ID
  then
    echo "${WIFI_ICON}$(echo $status | grep $WIFI_ID | cut -d: -f1)"
  else
    echo "${WIFI_ICON}-"
  fi
}

function render_vpn {
  local status=$1
  if echo $status | grep --perl-regexp --quiet 'tun\d'
  then
    echo "${VPN_ICON}vpn  "
  fi
}

wifi="$(render_wifi "$status")"
vpn=$(render_vpn "$status")

echo -en "%{A:battery:}${vpn}${wifi}%{A}%{T1}" > $OUTPUT
