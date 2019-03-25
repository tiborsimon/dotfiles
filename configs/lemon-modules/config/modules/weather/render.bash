#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

WEATHER_CACHE_FILE="./weather.cache"

function wait_for_connection {
  while true
  do
    if ping -c 1 1.1.1.1 &>/dev/null
    then
      break
    fi
    sleep 0.5
  done
}

function update_weather {
  response=$(http --pretty=format --print=hb wttr.in/?format="%t+%l")
  status=$(echo "$response" | head -n 1 | grep --only-matching --perl-regexp "\d\d\d")

  if [ "$status" == "200" ]
  then
    raw_weather=$(echo "$response" | tail -n 1)
    weather=$(echo $raw_weather | cut -d',' -f1 | awk '{ print $2, $1 }')
    echo "$weather" > $WEATHER_CACHE_FILE
  fi
}

wait_for_connection
update_weather

weather=$(cat $WEATHER_CACHE_FILE)

echo -en "%{T1}%{A:weather:}${weather}%{A}"
