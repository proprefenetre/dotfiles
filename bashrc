#! /usr/bin/env bash
#
# ~/.bashrc
#

# If not running interactively, don't do anything

[[ $- != *i* ]] && return

source "/usr/share/git/completion/git-prompt.sh"

[[ -n "$SSH_TTY" ]] && PS1="\[\e[1;35m\](SSH) \h \[\e[0m\]\w \$ " || PS1="\h \[\e[1;32m\]\w \[\e[0m\]\$(__git_ps1 '(%s)') \[\e[1;32m\]\$\[\e[0m\] "


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
[[ $PS1 && -f "/usr/share/fzf/key-bindings.bash" ]] && . "/usr/share/fzf/key-bindings.bash"
[[ $PS1 && -f "/usr/share/fzf/completion.bash" ]] && . "/usr/share/fzf/completion.bash"
