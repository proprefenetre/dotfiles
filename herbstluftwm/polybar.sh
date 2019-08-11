#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
ext_status=$(xrandr |  awk '/^DP1/ {print $2}')

polybar int &

if [[ "$ext_status" = "connected" ]]; then
    polybar ext &
fi
