#!/usr/bin/env bash

config=$HOME/.config
dotfiles_dir=$HOME/dotfiles
dest_home=(bashrc bash_aliases vimrc gitconfig gtkrc-2.0 i3blocks.conf tmux.conf xbindkeysrc xinitrc Xresources reminders urlview mutt)
dest_config=(libinput-gestures.conf redshift.conf gtk-3.0 i3 dunst)

# -h: true if exists and is symlink
for f in ${dest_home[@]}; do
    if [[ ! -h "$HOME/.$f" ]]; then 
        ln -s $dotfiles_dir/$f $HOME/.$f && echo "linked $f"
    fi
done

[[ ! -d "$config" ]] && mkdir $config

for f in ${dest_config[@]}; do
    if [[ ! -h "$config/$f" ]]; then
        ln -s $dotfiles_dir/$f $config/$f && echo "linked $f"
    fi
done
