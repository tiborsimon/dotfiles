#!/usr/bin/env bash

upower -e | grep BAT | xargs -I{} upower -i {} | grep -E "path|vendor|model|state|time to|percentage|capacity"
