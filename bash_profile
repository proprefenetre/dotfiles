#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

# environment
export HISTSIZE=10000
export HISTFILESIZE=20000
export PROMPT_COMMAND='history -a'

export EDITOR='gvim'
export PATH="${PATH}:$HOME/bin"
export BROWSER='chromium'
export LESS='-R'
export LESSOPEN='| /usr/bin/source-highlight-esc.sh %s'
export NLTK_DATA='~/Projects/corpus_analysis/nltk_data'
export WINEPREFIX='/home/niels/.wine'
