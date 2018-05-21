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

if [ "$(hostname)" = "archbook" ]; then
    libinput-gestures &
fi
if [ "$(hostname)" = "PFN" ]; then
    "$HOME"/bin/mhead -e
    xbindkeys &
fi

parcellite &
keeb -dv

mount drijf

xset +fp /usr/share/fonts/local
xset fp rehash
xset m 1 0

# urxvtd --quiet --fork --opendisplay

unclutter &

hsetroot -fill /home/niels/pics/sunset-originial.png

redshift &

emacs --daemon &

exec bspwm

# exec i3
