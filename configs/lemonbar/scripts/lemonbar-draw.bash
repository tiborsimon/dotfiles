#!/usr/bin/bash
cd $(dirname $(readlink -f $0))

NORMAL_FONT="%{T1}"
ICON_FONT="%{T2}"
ICON_FONT_SMALL="%{T3}"

function update {
  desktop=$(./modules/desktop.bash ${NORMAL_FONT} ${ICON_FONT})
  battery=$(./modules/battery.bash ${NORMAL_FONT} ${ICON_FONT_SMALL})
  clock=$(./modules/clock.bash ${NORMAL_FONT} ${ICON_FONT})

  echo -e "%{l} ${desktop}" \
          "%{c}${clock}" \
          "%{r} | ${battery}  "
}

trap update SIGUSR1
read

# while true; do
#   desktop=$(./modules/desktop.bash ${NORMAL_FONT} ${ICON_FONT})
#   battery=$(./modules/battery.bash ${NORMAL_FONT} ${ICON_FONT_SMALL})
#   clock=$(./modules/clock.bash ${NORMAL_FONT} ${ICON_FONT})

#   echo -e "%{l} ${desktop}" \
#           "%{c}${clock}" \
#           "%{r} | ${battery}  "
#   sleep 0.5
# done
