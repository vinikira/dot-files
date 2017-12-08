#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bars for all monitors
for monitor in $(xrandr | grep " connected " | awk '{ print$1 }'); do
    MONITOR=$monitor polybar example &
done




echo "Bars launched..."
