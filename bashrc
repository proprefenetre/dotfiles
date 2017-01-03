#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ -n "$SSH_TTY" ]] && PS1="\[\e[1;35m\](SSH)\h \[\e[0m\]\w \$ " || PS1="\h \[\e[1;32m\]\w \[\e[1;31m\]\[\e[0m\]\$ "

# shellcheck disable=SC2046
eval $(keychain --eval --agents gpg,ssh id_rsa)

# shopt
shopt -s checkwinsize
shopt -s extglob
shopt -s histappend

# aliases
# shellcheck disable=SC1090
[[ -f "$HOME"/.bash_aliases ]] && source "$HOME"/.bash_aliases

# bash completion
# shellcheck disable=SC1091
[[ $PS1 && -f /usr/share/bash-completion/bash_completion ]] && source /usr/share/bash-completion/bash_completion

# environment
export HISTSIZE=10000
export HISTFILESIZE=20000
export PROMPT_COMMAND='history -a'

export EDITOR='gvim'
export PATH="${PATH}:$HOME/bin"
export BROWSER='chromium'
export LESS='-R'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'
