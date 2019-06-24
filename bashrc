#! /usr/local/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1="\[\e[1;32m\]\w \$\[\e[0m\] "

# shellcheck disable=SC2046
eval $(keychain --eval --agents ssh,gpg id_rsa)

# shopt
shopt -s checkwinsize
shopt -s extglob
shopt -s histappend

# aliases
[[ -h "$HOME/.bash_aliases" ]] && . "$HOME/.bash_aliases"
[[ -f "/usr/local/etc/bash_completion" ]] && . "/usr/local/etc/bash_completion"

# environment variables
export HISTSIZE=50000
export HISTFILESIZE=50000
export PROMPT_COMMAND='history -a'
export CM_SELECTIONS=clipboard
export EDITOR='nvim'
export PATH="/usr/local/bin:/usr/local/sbin:$HOME/bin:$HOME/.poetry/bin/:${PATH}"
# export PATH="/usr/local/bin:/usr/local/sbin:$(brew --prefix)/opt/coreutils/libexec/gnubin:$HOME/bin:$HOME/.poetry/bin/:${PATH}"
# export PATH="$(brew --prefix)/opt/coreutils/libexec/gnubin:$HOME/bin:$HOME/.poetry/bin/:${PATH}"
export BROWSER='firefox-developer-edition'
export LESS='-R'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'

# prevent the annoying en_NL locale error
export LANG=en_US.UTF-8
# fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

# pyenv
# added to /etc/paths
# export PATH="/Users/niels/.pyenv/bin:$PATH"
# eval "$(pyenv init -)"
# eval "$(pyenv virtualenv-init -)"
