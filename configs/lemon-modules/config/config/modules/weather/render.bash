#!/usr/bin/env bash
cd $(dirname $(readlink -f $0))

OUTPUT=$1

echo -en "$(python weather.py)" > $OUTPUT
