#!/usr/bin/env bash

config=~/.config
dotfiles_dir=~/.dotfiles
dest_home=(bashrc bash_aliases vimrc gitconfig gtkrc-2.0 i3blocks.conf snippyrc tmux.conf xbindkeysrc xinitrc Xresources reminders urlview mutt)
dest_config=(libinput-gestures.conf redshift.conf gtk-3.0 i3 dunst)

for f in ${dest_home[@]}; do
    if [[ ! -h "$HOME/.$f" ]]; then
        echo "ln -s $dotfiles_dir/$f --> $HOME/.$f"
    fi
done

for f in ${dest_config[@]}; do
    if [[ ! -d "$config" ]]; then
        echo "mkdir $config" 
    fi
    
    if [[ ! -h "$config/$f" ]]; then
        echo "ln -s $dotfiles_dir/$f --> $HOME/.config/$f"
    fi
    # ln -s "$dotfiles_dir/$f" "$HOME/.config/$f" && 
done
