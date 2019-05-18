#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bars for all monitors
xrandr | grep " connected " | awk '{ print $1 " " $3 }' |
    while read -r monitor
    do
        SCREEN_NAME=$(echo "$monitor" | awk '{ print $1 }')
        SCREEN_STATUS=$(echo "$monitor" | awk '{ print $2 }')

        if [ "$SCREEN_STATUS" == "primary" ]; then
            MONITOR=$SCREEN_NAME polybar primary-top &
        else
            MONITOR=$SCREEN_NAME polybar secondary-top &
        fi
    done

echo "Bars launched..."
