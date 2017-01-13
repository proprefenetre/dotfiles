#!/usr/bin/env bash

df_dir=$PWD
dest_home=(bashrc bash_aliases vimrc gitconfig gtkrc-2.0 i3blocks.conf snippyrc tmux.conf xbindkeysrc xinitrc Xresources reminders urlview mutt)
dest_config=(libinput-gestures.conf redshift.conf gtk-3.0 i3 dunst)

for f in ${dest_home[@]}; do
    cd $HOME
    ln -s "$df_dir/$f" ".$f" && echo "linked $df_dir/$f --> .$f"
done

for f in ${dest_config[@]}; do
    [[ ! -d "$HOME/.config" ]] && mkdir "$HOME/.config"
    cd $HOME
    ln -s "$df_dir/$f" ".config/$f" && echo "linked $df_dir/$f --> .config/$f"
done
