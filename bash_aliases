#!/usr/bin/env bash

alias amke='make'
alias bashrc='emacsclient -n -c ~/.bashrc'
alias battlenet='wine .wine/drive_c/Program\ Files\ \(x86\)/Battle.net/Battle.net.exe'
alias bc='bc ~/dotfiles/funcs.bc -l'
alias cal='cal -3w --color=always'
alias cc='gcc -Wall -Wextra -pedantic -g -std=c11'
alias cd..='cd ..'
alias ch='cd'
alias cignore='curl https://www.gitignore.io/api/vim -o .gitignore'
alias cp='cp -v'
alias dd='dd status=progress'
alias detwist='pacman -Rns $(pacman -Qtdq)' # remove orphans
alias df='df -h'
alias dnf='sudo dnf'
alias drijf='cadaver https://proprefenetre.stackstorage.com/remote.php/webdav/'
alias dropbox='dropbox-cli'
alias ducks='du -cksh * | sort -hr | head -n 15'
alias dunstr='killall dunst && dunst &'
alias e='emax'
alias ex-start='npm install && touch .projectile'
alias get='fc -s "Ss=S"'
alias gg='g++ -Wall -Wextra -std=gnu++11 -g'
alias g='emax'
alias grep='grep --color=auto'
alias i3conf='emacsclient -n -c ~/.config/i3/config'
alias ltst='ls -lt | head -6'
alias ls='ls --color=always'
alias mak='make'
alias mkae='make'
alias mkdir='mkdir -p -v'
alias mv='mv -i'
alias myip='echo $(curl -s http://ipecho.net/plain)'
alias myports='nmap -sT 127.0.0.1'
alias netctl-auto='sudo netctl-auto'
alias nw='urxvtc -cd $PWD'
alias octoperms='stat -c "%a %n"'
alias pacman='sudo pacman --color=always'
alias pakku='pakku --color=always'
alias pdf-tif='gs -dNOPAUSE -r300 -sDEVICE=tiffg4 -sOutputFile=vdmast.tif'
alias pgp='gpg'
alias pignore='curl https://www.gitignore.io/api/python%2Cemacs -o .gitignore'
alias ping3='ping -c3 www.google.com'
alias pkglist='pacman -Qqe >pkglist'
alias powertop='sudo powertop'
alias py='ipython'
alias rmlinks='find -L . -name . -o -type d -prune -o -type l -exec rm {} + '
alias Rv='vim --servername VIM'
alias scummvm='SDL_AUDIODRIVER=dummy scummvm'
alias se='sudo emacsclient -n -c'
alias setxkb='setxkbmap -print -v 10'
alias shutdown='shutdown.sh'
alias src='source ~/.bashrc && echo "sourced bashrc"'
alias startx-logged='startx > /home/niels/logs/startx-$(date +%d%m%y-%H:%M).log'
alias starwars='telnet towel.blinkenlights.nl'
alias steam='primusrun steam-native'
alias steam32='WINEARCH=win32 WINEPREFIX=~/win32 wine ~/win32/drive_c/Program\ Files/Steam/Steam.exe'
alias trea='tree -apugCL 1'
alias tree='tree -C'
alias trtunnel='ssh -L 9091:localhost:9091 laptop'
alias vignore='curl https://www.gitignore.io/api/vim -o .gitignore'
alias vt-vim='urxvt -e vim'
alias v='vim'
alias zath='zathura --fork'
alias startx='exec startx'
