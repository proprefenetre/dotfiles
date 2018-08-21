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
    hsetroot -fill /home/niels/pics/wallpapers/lighthouse.jpg &
    keeb -dvc &
    # low power level warning for until I've fixed a panel
    battery3 &

fi
if [ "$(hostname)" = "PFN" ]; then
    "$HOME"/bin/mhead -e
    hsetroot -fill /home/niels/pics/sunset-originial.png &
    keeb -dv
fi

mount drijf &

xset +fp /usr/share/fonts/local
xset fp rehash
xset m 1 0

unclutter &

redshift &
emacs --daemon &

panel &
sxhkd &

exec bspwm
