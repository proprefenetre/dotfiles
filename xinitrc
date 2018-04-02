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

if [ "$(hostname)" = 'archbook' ]; then
    # setxkbmap  -layout us -model chromebook -variant dvorak-alt-intl -option altwin:swap_alt_win
    "$HOME"/bin/keeb -dv
    libinput-gestures &
fi
if [ "$(hostname)" = 'pfn' ]; then
    "$HOME"/bin/mhead
    # setxkbmap  -layout us -model asus_laptop -variant dvorak-alt-intl -option altwin:swap_alt_win
    parcellite &

    "$HOME"/bin/keeb -dv
    xbindkeys &
fi


mount drijf

unison org >/dev/null 2>&1 &

xset +fp /usr/share/fonts/local
xset fp rehash
xset m 1 0

urxvtd --quiet --fork --opendisplay

unclutter &

/home/niels/bin/setbg &
# xautolock -locker "systemctl suspend -i" -time 15 -detectsleep &

redshift &

# remind_sync.sh &
# remind -z -k'notify-send -a reminder %s' .reminders &

exec i3
