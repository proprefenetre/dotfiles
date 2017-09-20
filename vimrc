" vimrc -- Sinterklaas iteration

" Preamble
" ========
set nocompatible
filetype plugin indent on
syntax enable

call plug#begin('~/.vim/plugged')

Plug 'sirver/ultisnips'

Plug 'proprefenetre/molokai'
Plug 'altercation/vim-colors-solarized'

Plug 'vim-syntastic/syntastic'
Plug 'airblade/vim-gitgutter'

Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'

Plug 'ctrlpvim/ctrlp.vim'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-eunuch'

Plug 'godlygeek/tabular'

Plug 'junegunn/rainbow_parentheses.vim'

Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc-after'

Plug 'ron89/thesaurus_query.vim'
Plug 'reedes/vim-wordy'

call plug#end()

" General
" =======
let g:molokai_original = 1
let g:rehash256 = 1
colorscheme molokai

hi clear SignColumn	

if has ("gui_running")
    set guifont=Hack\ 10
    set go-=e
    set go-=m
    set go-=r
    set go-=L
    set go-=T
    set go+=a
    set go+=c
    set guiheadroom=0
endif

set backspace=indent,eol,start
set mouse=a

set encoding=utf-8

set number
set showcmd
set wildmenu
set wildmode=longest:list,full
set concealcursor=c
set showmatch
set scrolloff=999
set textwidth=79
set cc=+1               
set fo+=j
set splitright

" folds
" -----
set foldmethod=indent
set foldnestmax=10

" statusline
" ----------
set laststatus=2
set ruler
set statusline=%f\ %h%w%m%r\ 
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%=%(%l,%c%V\ %=\ %P%) 

" auto-anything
" -------------
set autochdir
set autowriteall
set autoread
set autoindent

" searching
" ---------
set ignorecase
set incsearch
set smartcase
set nohlsearch
set incsearch
set gdefault

" spelling 
" --------
set spelllang=en,nl

" general style
" -------------
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab

set formatprg=par\ -w80q

" swp/backup files
" ================
set backupdir=./.backup,.,/tmp
set directory=./.swap,.,/tmp

" File-type specific
" ==================

" *unspecific
" -----------
autocmd BufNewFile,BufRead * :RainbowParentheses

" C(++)
" -----
augroup c
    autocmd!
    autocmd FileType c set cms=//\ %s
    autocmd FileType cpp set cms=//\ %s
augroup END

" MDPP
" ----
autocmd BufNewFile,BufRead *.mdpp set filetype=pandoc


" Functions
" =========

" fill line with character(s) {{{
" ---------------------------
function! FillLine(str)
    let tw = 79
    .s/[[:space:]]*$//
    let reps = (tw - col("$")) / len(a:str)
    if reps > 0
        .s/$/\=(' '.repeat(a:str, reps))/
    endif
endfunction

command! -nargs=1 Fill call FillLine(<args>) 
" }}} "

" toggle ui elements {{{
" ------------------
let g:toggle_tabline=0
let g:hide_all=0

function! ToggleHideAll()
    if g:hide_all
        let g:toggle_tabline=0
        let g:hide_all=0
        " set number
        hi LineNr guifg=#BCBCBC
        set numberwidth=4
        set cc=80
        set showmode
        set ruler
        set laststatus=2
        set showcmd
        set showtabline=1
    else
        let g:toggle_tabline=1
        let g:hide_all=1
        " set nonumber
        hi LineNr guifg=bg
        set numberwidth=4
        set cc=
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
        set showtabline=0
    endif
endfunction

function! ToggleTabline()
    if g:toggle_tabline
        let g:toggle_tabline=0
        set showtabline=1
    else
        let g:toggle_tabline=1
        set showtabline=0
    endif
endfunction
" }}} "

" create command aliases {{{
" ----------------------
function! CommandAlias(key, value)
    exe printf('cabbrev <expr> %s (getcmdtype() == ":" && getcmdpos() <= %d) ? %s : %s', a:key, 1+len(a:key), string(a:value), string(a:key))
endfu
" }}} "

" 'run' mapping {{{
" -------------
function! MapR()
    if (&ft=='pandoc')
        write
        make
    elseif (&ft=='python')
        write
        !python %
    elseif (&ft=='c')
        write
        make
    else
        write
    endif
endfunction
" }}} "

" Plugins
" =======

" ctlp
" ----
" let g:ctrlp_switch_buffer='et'
let g:ctrlp_cmd = 'CtrlP'

" UltiSnips
" ---------
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsSnippetDirectories=["snipperts"]
let g:UltiSnipsEditSplit="vertical"

" Rainbow parentheses
" -------------------
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]

" vim-easytags
" ------------
set tags=./tags;,~/.vimtags
let g:easytags_events = ['BufReadPost', 'BufWritePost']
let g:easytags_async = 1
let g:easytags_dynamic_files = 2
let g:easytags_resolve_links = 1
let g:easytags_suppress_ctags_warning = 1

" vim-pandoc
" ----------
let g:pandoc#filetyes#handled = ["markdown", "mail"]
let g:pandoc#folding#level = 1
let g:pandoc#folding#fdc = 0
let g:pandoc#formatting#mode = "h"

let g:pandoc#after#modules#enabled = ["ultisnips"]

" thesaurus_query
" ---------------
let g:tq_enabled_backends=["thesaurus_com","mthesaur_txt"]

" vim-vinegar
" -----------
let g:netrw_list_hide='\(^\|\s\s\)\zs\.\S\+'

" syntastic
" ---------
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:syntastic_python_checkers = ["flake8"]
let g:syntastic_python_flake8_args = "--ignore=E501"

let g:syntastic_bash_checkers = ["ShellCheck"]
let g:syntastic_sh_ShellCheck_args = "-x"

" Mappings
" ========

let mapleader = ","

" navigation
map j gj
map k gk

inoremap jj <esc>

" windows 'n splits
nnoremap <c-w><c-v> :vne<cr>
nnoremap <c-w><c-t> :tabedit<cr>

nmap <C-d> <C-w><C-h>
nmap <C-h> <C-w><C-j>
nmap <C-t> <C-w><C-k>
nmap <C-n> <C-w><C-l>

" replace
noremap <leader>gr :%s/\<<C-r><C-w>\>/
nnoremap <leader>lr :s/<C-r><C-w>/
vnoremap <leader>lr y:s/<C-r>"/

" copy pasta
nnoremap <leader>p "+p
vnoremap <leader>y "+y

" editing vimrc
nnoremap <leader>.p :vs $MYVIMRC<cr>
nnoremap <leader>eu :source $MYVIMRC<cr>

" formatting
nnoremap Q vapgq
vnoremap Q gq

" highlight information
map <F3> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '>
            \ trans<' . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" .
            \ synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">" .
            \ " FG:" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"fg#")<cr>

" word count
nnoremap <leader>wc :!wc -w % <bar> cut -d\  -f1<cr>

" 'run' mapping
nnoremap <leader>r :call MapR()<cr>

" hide statusline, linenumbers etc.
nnoremap <silent> <s-h> :call ToggleHideAll()<cr>
nnoremap <silent> <s-t> :call ToggleTabline()<cr>

" command line keys
cnoremap <c-a> <Home>

" vim-fugitive
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gs :Gstatus<cr>

" aliases
call CommandAlias("reindent", ":!indent -kr %")
call CommandAlias("W", "w")
call CommandAlias("Q","q")
call CommandAlias("Wq","wq")
call CommandAlias("Sx", ":SudoWrite<cr> :q<cr>")

" write as root
command! Sw :SudoWrite
