#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        # shellcheck disable=SC1090
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

[ -L "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"
[ -L "$HOME/.Xmodmap" ] && xmodmap "$HOME/.Xmodmap"

xset +fp /usr/share/fonts/local
xset fp rehash
xset m 1 0

unclutter &
redshift &
# hsetroot -fill 

mhead -i &
keeb -d &
rat.sh &
dunst &

xidlehook --not-when-fullscreen \
          --not-when-audio \
          --timer normal 180 \
          'xbacklight -set 5' \
          'xbacklight -set 50' \
          --timer primary 300 \
          'xset dpms force off; lock' 'xbacklight -set 50' &

exec bspwm
