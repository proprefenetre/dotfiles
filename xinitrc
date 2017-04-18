#!/bin/sh
#
# starts 
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -f "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"

[ "$HOSTNAME" == 'arch' ] && mhead -e
xset +fp /usr/share/fonts/local
xset fp rehash
xset m 1 0

# for xdotool:
setxkbmap  -layout us -model chromebook -variant dvorak-alt-intl &
urxvtd -q -f -o
unclutter &
# hsetroot -fill $(shuf -n 1 -e "$HOME/files/pics/wallpaper/*") &
/home/niels/bin/setbg &
# xautolock -locker "systemctl suspend -i" -time 15 -detectsleep &
redshift &
libinput-gestures &
archupdates &
~/bin/focus-last.py &

mount ~/drijf &
remind_sync.sh &
remind -z -k'notify-send -a reminder %s' .reminders &

exec i3
