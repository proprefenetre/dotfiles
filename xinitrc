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

mhead
setxkbmap -model chromebook \
       	-layout us \
	-variant dvorak-alt-intl \
 	-option terminate:ctrl_alt_bksp \
 	-option ctrl:swap_lwin_lctl \
	-option compose:rctrl
 
xautolock -time 10 -locker slock &
sxhkd -m -1 &

exec bspwm
