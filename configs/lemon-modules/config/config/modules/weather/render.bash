#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

OUTPUT=$1

WEATHER_CACHE_FILE="./.weather.cache"

function wait_for_connection {
  while true
  do
    if ping -c 1 1.1.1.1 &>/dev/null
    then
      echo "Network available."
      break
    fi
    echo "Waiting for network.."
    sleep 0.5
  done
}

function update_weather {
  response=$(http --pretty=format --print=hb get wttr.in/?format="%t")
  status=$(echo "$response" | head -n 1 | grep --only-matching --perl-regexp "\d\d\d")

  if [ "$status" == "200" ]
  then
    weather=$(echo "$response" | tail -n 1)
    echo "$weather" > $WEATHER_CACHE_FILE
    echo "Weather updated: ${weather}."
  else
    echo "Weather cannot be updated. Response status: $status"
  fi
}

wait_for_connection
update_weather

weather=$(cat $WEATHER_CACHE_FILE)

echo -en "%{T1}%{A:weather:}%{T5}杖 %{T1}${weather}%{A}" > $OUTPUT
