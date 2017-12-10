#!/bin/bash

# functions
connect () {
    ssid="$1"
    wifi_id=$(connmanctl services | grep $ssid | awk '{print $4}')
    connmanctl connect $wifi_id
}

gvim () { 
    command gvim --remote-tab-silent "$@" >/dev/null 2>&1 || command gvim "$@" ; 
}

clean_path () {
    PATH=$(echo "$PATH" | awk -v RS=':' -v ORS=":" '!a[$1]++{if (NR > 1) printf ORS; printf $a[$1]}')
}

# dnf
alias dnf='sudo dnf'

# dunst
alias dunstr='killall dunst && dunst &'

#dropbox
alias dropbox='dropbox-cli'

# sudo
alias pacman='pacaur'
# alias systemctl='sudo systemctl'
alias netctl-auto='sudo netctl-auto'

# python
alias py='python'

## gcc
alias gg='g++ -Wall -Wextra -std=gnu++11 -g'
alias cc='gcc -Wall -Wextra -pedantic -g -std=c11'

## vim
alias g='gvim'
alias v='vim'
alias vt-vim='urxvt -e vim'
# R/vimcom
alias Rv='vim --servername VIM'

## configs
alias bashrc='gvim ~/.bashrc'
alias i3conf='gvim ~/.config/i3/config'
alias src='source ~/.bashrc && echo "sourced bashrc"'

## commands
alias battlenet='wine .wine/drive_c/Program\ Files\ \(x86\)/Battle.net/Battle.net.exe'
alias cal='cal -3w --color=always'
alias cignore='curl https://www.gitignore.io/api/c%2Cvim -o .gitignore'
alias cp='cp -v'

alias dd='dd status=progress'
alias detwist='pacman -Rns $(pacman -Qtdq)' # remove orphans
alias df='df -h'
alias drijf='cadaver https://proprefenetre.stackstorage.com/remote.php/webdav/'
alias ducks='du -cksh * | sort -hr | head -n 15'

alias grep='grep --color=auto'

alias ls='ls --color=always'

alias mkdir='mkdir -p -v'
alias myip='echo $(curl -s http://ipecho.net/plain)'
alias myports='nmap -sT 127.0.0.1'

alias nw='urxvtc -cd $PWD'

alias octoperms='stat -c "%a %n"'

alias pdf-tif='gs -dNOPAUSE -r300 -sDEVICE=tiffg4 -sOutputFile=vdmast.tif'
alias pignore='curl https://www.gitignore.io/api/python%2Cvim -o .gitignore' 
alias ping3='ping -c3 www.google.com'
alias powertop='sudo powertop'

alias rmlinks='find -L . -name . -o -type d -prune -o -type l -exec rm {} + '

alias setxkb='setxkbmap -print -v 10'
alias scummvm='SDL_AUDIODRIVER=dummy scummvm'
alias starwars='telnet towel.blinkenlights.nl'
alias steam32='WINEARCH=win32 WINEPREFIX=~/.win32 wine ~/.win32/drive_c/Program\ Files/Steam/Steam.exe'

alias tr='transmission-remote'
alias tree='tree -C'
alias trea='tree -apugCL 1'
alias trtunnel='ssh -L 9091:localhost:9091 laptop'

alias vignore='curl https://www.gitignore.io/api/vim -o .gitignore' 
# alias ls='tree -CL 1'

alias wakey='ssh server -t ~/bin/wake'

## mistakes
alias cd..='cd ..'
alias ch='cd'
alias pgp='gpg'
alias amke='make'
alias mak='make'
alias mkae='make'
