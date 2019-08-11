#! /usr/bin/env bash

id=$(xinput | rg -i ".*kensington.*id=([0-9]+).*" -r '$1')

if [[ -f /tmp/precision-mode ]]; then
    xinput set-prop "$id" 'libinput Accel Speed' 0
    rm -f /tmp/precision-mode
    notify-send -a "trackball" "precision mode" "off"
else
    xinput set-prop "$id" 'libinput Accel Speed' -0.95
    touch /tmp/precision-mode
    notify-send -a "trackball" "precision mode" "on"
fi
