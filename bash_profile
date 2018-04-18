#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# environment
export HISTSIZE=50000
export HISTFILESIZE=50000
export PROMPT_COMMAND='history -a'

export EDITOR='emacsclient -n -c'
export ALTERNATE_EDITOR=""
export PATH="${PATH}:$HOME/bin:$HOME/.cargo/bin"
export BROWSER='chromium'
export LESS='-R'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'
