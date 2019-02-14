#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

# loading config with the predefined font placeholders
source ./config.bash

if [[ ! -p $LEMONBAR_NAMED_PIPE ]]; then
  echo "ERROR: Lemonbar pipe $LEMONBAR_NAMED_PIPE not found!"
  exit 1
fi

MODULE_CONFIG_PATH="./config.bash"

desktop=$(./modules/desktop.bash ${MODULE_CONFIG_PATH})
battery=$(./modules/battery.bash ${MODULE_CONFIG_PATH})
clock=$(./modules/clock.bash ${MODULE_CONFIG_PATH})

echo -e "%{l} ${desktop}" \
        "%{c}${clock}" \
        "%{r} ${battery}  " > $LEMONBAR_NAMED_PIPE
