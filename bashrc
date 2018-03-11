#! /usr/bin/env bash
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

# see .bash_profile for environment variables
