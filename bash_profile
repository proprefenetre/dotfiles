#! /usr/bin/env bash
#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# environment
export HISTSIZE=50000
export HISTFILESIZE=50000
export PROMPT_COMMAND='history -a'
export CM_SELECTIONS=clipboard
export EDITOR='emax'
export ALTERNATE_EDITOR=""
export PATH="${PATH}:$HOME/bin"
export BROWSER='firefox-developer-edition'
export LESS='-R'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'

# bspwm
export PANEL_FIFO=/tmp/panel-fifo
export PANEL_HEIGHT=24
export PANEL_FONT="-*-hack-*-*-*-*-10-*-*-*-*-*-*-*"
export PANEL_WM_NAME=bspwm_panel
