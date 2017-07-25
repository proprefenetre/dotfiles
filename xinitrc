#!/bin/sh
#
# starts 
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -L "$HOME/.Xresources" ] && xrdb -merge "$HOME/.Xresources"
[ -L "$HOME/.Xmodmap" ] && xmodmap "$HOME/.Xmodmap"



if [ $HOSTNAME == 'archbook' ]; then
    setxkbmap  -layout us -model chromebook -variant dvorak-alt-intl
else
    $HOME/bin/mhead -e
    setxkbmap  -layout us -model asus_laptop -variant dvorak-alt-intl
    xbindkeys &
fi
    
xset +fp /usr/share/fonts/local
xset fp rehash
xset m 1 0

urxvtd -q -f -o
unclutter &

# hsetroot -fill $(shuf -n 1 -e "$HOME/files/pics/wallpaper/*") &
/home/niels/bin/setbg &
# xautolock -locker "systemctl suspend -i" -time 15 -detectsleep &

redshift &
libinput-gestures &
archupdates &
~/bin/focus-last.py &

remind_sync.sh &
remind -z -k'notify-send -a reminder %s' .reminders &

exec i3
