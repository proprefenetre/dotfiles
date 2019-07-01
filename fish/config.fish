#! /usr/local/bin/fish

set PATH /usr/local/bin /usr/local/sbin /Users/niels/.poetry/bin/ $PATH

set EDITOR "nvim"
set BROWSER "firefox-developer-edition"
set LESS "-R"
set LESSOPEN '| /usr/bin/source-highlight-esc.sh %s'

set FZF_DEFAULT_COMMAND 'fd --type f'
set FZF_CTRL_T_OPTS "--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
set FZF_ALT_C_OPTS  "--preview 'tree -C {} | head -200'"

set LANG "en_US.UTF-8"
