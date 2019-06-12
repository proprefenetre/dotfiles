#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        # shellcheck disable=SC1090
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

[ -L "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"

xset +fp /usr/share/fonts/local
xset fp rehash
xset m 1 0

# mount drijf &

unclutter &
redshift &

mhead &
hsetroot -full /home/niels/Downloads/spruiten.jpg &

remind -z -k'notify-send -a "Reminder" %s &' /home/niels/.reminders &

dunst &

setxkbmap -model pc104 \
          -layout us \
          -variant dvorak-alt-intl \
          -option \
          -option terminate:ctrl_alt_bksp \
          -option compose:rctrl \
          -option ctrl:nocaps

xidlehook \
    --not-when-fullscreen \
    --not-when-audio \
    --timer normal 60 \
    'xbacklight -set 10' \
    'xbacklight -set 30' \
    --timer primary 240 \
    'xbacklight -set 30; slock' \
    '' \
    --timer normal 1800 \
    'systemctl hibernate' \
    '' &

firefox-developer-edition &

touchegg &

sxhkd -m -1 &
exec bspwm
