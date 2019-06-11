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

mount drijf &

unclutter &
redshift &
hsetroot -fill /home/niels/pics/DE-foto.jpg &

mhead &
dunst &
sxhkd -m -1 &

xidlehook --not-when-fullscreen \
          --not-when-audio \
          --timer normal 600 \
          'xbacklight -set 5' \
          'xbacklight -set 70' \
          --timer primary 30 \
          'xbacklight -set 70; slock' '' &

exec bspwm
