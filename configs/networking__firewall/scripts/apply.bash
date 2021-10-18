#!/usr/bin/env bash

function set_up_networking_stack {
  #DROP ICMP echo-requests sent to broadcast/multi-cast addresses.
  echo 1 > /proc/sys/net/ipv4/icmp_echo_ignore_broadcasts

  #DROP source routed packets
  echo 0 > /proc/sys/net/ipv4/conf/all/accept_source_route

  #Enable TCP SYN cookies
  echo 1 > /proc/sys/net/ipv4/tcp_syncookies

  #Do not ACCEPT ICMP redirect
  echo 0 > /proc/sys/net/ipv4/conf/all/accept_redirects

  #Don't send ICMP redirect
  echo 0 >/proc/sys/net/ipv4/conf/all/send_redirects

  #Enable source spoofing protection
  echo 1 > /proc/sys/net/ipv4/conf/all/rp_filter

  #Log impossible (martian) packets
  echo 1 > /proc/sys/net/ipv4/conf/all/log_martians
}


iptables --flush

iptables --policy INPUT   DROP
iptables --policy FORWARD DROP
iptables --policy OUTPUT  DROP

# http://www.chiark.greenend.org.uk/~peterb/network/drop-vs-reject
iptables --new-chain LOG_AND_DROP
iptables --new-chain LOG_AND_REJECT

iptables --new-chain TCP_OUT
iptables --new-chain UDP_OUT


# =================================================================================================
#  I N P U T   C H A I N

iptables --append INPUT --in-interface lo --jump ACCEPT
iptables --append INPUT --match conntrack --ctstate ESTABLISHED,RELATED --jump ACCEPT
iptables --append INPUT --jump  LOG_AND_DROP


# =================================================================================================
#  F O R W A R D   C H A I N

iptables --append FORWARD --jump LOG_AND_DROP


# =================================================================================================
#  O U T P U T   C H A I N

iptables --append OUTPUT --out-interface lo --jump ACCEPT
iptables --append OUTPUT --match conntrack --ctstate ESTABLISHED,RELATED --jump ACCEPT

iptables --append OUTPUT --protocol udp       --match conntrack --ctstate NEW --jump UDP_OUT
iptables --append OUTPUT --protocol tcp --syn --match conntrack --ctstate NEW --jump TCP_OUT

iptables --append OUTPUT --protocol udp --jump REJECT --reject-with icmp-port-unreachable
iptables --append OUTPUT --protocol tcp --jump REJECT --reject-with tcp-reset

iptables --append OUTPUT --jump  LOG_AND_REJECT


# =================================================================================================
#  L O G G I N G   C H A I N S

iptables --append LOG_AND_DROP --match limit --limit 5/min --log-prefix "iptables-dropped: " --log-level 4 --jump LOG
iptables --append LOG_AND_DROP --jump DROP

iptables --append LOG_AND_REJECT --match limit --limit 5/min --log-prefix "iptables-rejected: " --log-level 4 --jump LOG
iptables --append LOG_AND_REJECT --jump REJECT --reject-with icmp-proto-unreachable


# =================================================================================================
#  U D P   O U T   C H A I N

iptables --append UDP_OUT --dport 67:68 --jump ACCEPT  # DHCP
iptables --append UDP_OUT --dport 53    --jump ACCEPT  # DNS


# =================================================================================================
#  T C P   O U T   C H A I N

iptables --append TCP_OUT --dport 53  --jump ACCEPT  # DNS
iptables --append TCP_OUT --dport 80  --jump ACCEPT  # HTTP
iptables --append TCP_OUT --dport 110 --jump ACCEPT  # POP
iptables --append TCP_OUT --dport 443 --jump ACCEPT  # HTTPS
