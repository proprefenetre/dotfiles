#! /usr/local/bin/bash
#
# ~/.bashrc
#

# If not running interactively, don't do anything

[[ $- != *i* ]] && return

[[ -n "$SSH_TTY" ]] && PS1="\[\e[1;35m\](SSH) \h \[\e[0m\]\w \$ " || PS1="\h \[\e[1;32m\]\w \$\[\e[0m\] "


# shellcheck disable=SC2046
eval $(keychain --eval --agents ssh,gpg id_rsa)

# shopt
shopt -s checkwinsize
shopt -s extglob
shopt -s histappend

# aliases
# shellcheck disable=SC1090
[[ -f "$HOME/.bash_aliases" ]] && . "$HOME/.bash_aliases"

# bash completion
# shellcheck disable=SC1091
[[ $PS1 && -f "/usr/share/bash-completion/bash_completion" ]] && .  "/usr/share/bash-completion/bash_completion"

# environment variables
export HISTSIZE=50000
export HISTFILESIZE=50000
export PROMPT_COMMAND='history -a'
export CM_SELECTIONS=clipboard
export EDITOR='vim'
export PATH="/usr/local/bin:/usr/local/sbin:$(brew --prefix)/opt/coreutils/libexec/gnubin:$HOME/bin:$HOME/.poetry/bin/:${PATH}"
export BROWSER='firefox-developer-edition'
export LESS='-R'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'


# fzf
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
export FZF_DEFAULT_COMMAND='fd --type f'
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"
