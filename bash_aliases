#! /usr/local/bin/bash

# gnu coreutils
alias ls='gls --color=always'
alias mv='gmv'
alias cp='gcp -v'
alias dd='gdd status=progress'
alias du='gdu'

# spelling
alias amke='make'
alias cd..='cd ..'
alias ch='cd'
alias pgp='gpg'

# de rest
alias bc='bc ~/dotfiles/funcs.bc -l'
alias cal='cal -3w --color=always'
alias cata='DYLD_LIBRARY_PATH=. DYLD_FRAMEWORK_PATH=.; cd "/Applications/Cataclysm.app/Contents/Resources" && ./cataclysm; exit'
alias cc='gcc -Wall -Wextra -pedantic -g -std=c11'
alias cignore='curl https://www.gitignore.io/api/vim -o .gitignore'
alias d='docker'
alias df='df -h'
alias dnf='sudo dnf'
alias ducks='gdu -cksh ./* | sort -hr | head -n 15'
alias get='fc -s "Ss=S"'
alias gg='g++ -Wall -Wextra -std=gnu++11 -g'
alias grep='grep --color=auto'
alias myip='echo $(curl -s http://ipecho.net/plain)'
alias myports='nmap -sT 127.0.0.1'
alias nv="nvim"
alias octoperms='stat -c "%a %n"'
alias pdf-tif='gs -dNOPAUSE -r300 -sDEVICE=tiffg4 -sOutputFile=vdmast.tif'
alias pignore='curl https://www.gitignore.io/api/python%2Cemacs -o .gitignore'
alias ping3='ping -c3 www.google.com'
alias py='ipython'
alias rmlinks='find -L . -name . -o -type d -prune -o -type l -exec rm {} + '
alias scummvm='SDL_AUDIODRIVER=dummy scummvm'
alias ssh-finger='ssh-add -l -E md5'
alias src='source ~/.bashrc && echo "sourced bashrc"'
alias starwars='telnet towel.blinkenlights.nl'
alias trea='tree -apugCL 1'
alias tree='tree -C'
alias trtunnel='ssh -L 9091:localhost:9091 laptop'
alias vex="xev | sed -n -e 's/.*(\(keysym .\+\)).*/\1/p'"
alias vignore='curl https://www.gitignore.io/api/vim -o .gitignore'
alias v='nvim'
alias vim='nvim'
