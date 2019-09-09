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
export PATH="${PATH}:$HOME/bin:$HOME/.cargo/bin:$HOME/.local/bin:$HOME/.poetry/bin"
export BROWSER='firefox-developer-edition'
export LESS='-R'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export FZF_DEFAULT_COMMAND='fd --type f'
