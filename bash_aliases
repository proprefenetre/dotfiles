#!/bin/bash

# dunst
alias dunstr='killall dunst && dunst &'

wcw () 
{
    wc -w "$1" | cut -d\  -f1
}

# wc
#dropbox
alias dropbox='dropbox-cli'

# sudo
alias pacman='sudo pacman'
# alias systemctl='sudo systemctl'
alias netctl-auto='sudo netctl-auto'

# python
alias py='python'

## activate pyvenv
activate ()
{
    source "$1/bin/activate"
}

## feh
alias feh='feh -g 640x480 -d'

## gcc
alias gg='g++ -Wall -Wextra -std=gnu++11 -g'
alias cc='gcc -Wall -Wextra -pedantic -g -std=c11'

## R/vimcom
alias Rv='vim --servername VIM'

## vim
alias g='gvim'
alias v='vim'

## configs
alias bashrc='gvim ~/.bashrc'
alias i3conf='gvim ~/.config/i3/config'

## misc
alias cp='cp -v'
alias ls='ls --color=always'
alias trea='tree -apugCL 1'
alias tree='tree -C'
alias grep='grep --color=auto'
alias ping3='ping -c3 www.google.com'
alias mkdir='mkdir -p -v'
alias cal='cal -3w --color=always'
alias pdf-tif='gs -dNOPAUSE -r300 -sDEVICE=tiffg4 -sOutputFile=vdmast.tif'
alias starwars='telnet towel.blinkenlights.nl'
alias src='source ~/.bashrc && echo "sourced bashrc"'
alias cd..='cd ..'
alias ch='cd'
alias setxkb='setxkbmap -print -v 10'
alias vt-vim='urxvt -e vim'
alias pgp='gpg'
alias df='df -h'
alias powertop='sudo powertop'
alias nw='urxvtc -cd $PWD'
alias yup='git -c help.autocorrect=-1 $(history -p !*)'
alias pignore='curl https://www.gitignore.io/api/python -o .gitignore' 
alias cignore='curl https://www.gitignore.io/api/c -o .gitignore'
alias tr='transmission-remote'
