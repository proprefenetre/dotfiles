" vimrc -- Sinterklaas iteration

" Preamble
" ========

set nocompatible
filetype plugin indent on
syntax enable

call plug#begin('~/.vim/plugged')

Plug 'sirver/ultisnips'

Plug 'vim-airline/vim-airline', { 'for': ['python', 'c', 'cpp', 'sh'] }
Plug 'vim-airline/vim-airline-themes', { 'for': ['python', 'c', 'cpp', 'sh'] }

Plug 'proprefenetre/molokai'
Plug 'altercation/vim-colors-solarized'

Plug 'vim-syntastic/syntastic', { 'for': ['python', 'c', 'cpp', 'sh'] }
Plug 'airblade/vim-gitgutter'

" Plug 'xolox/vim-misc', { 'for': ['python', 'c', 'cpp', 'sh'] }
" Plug 'xolox/vim-easytags', { 'for': ['python', 'c', 'cpp'] }"
Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'

Plug 'ctrlpvim/ctrlp.vim'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'

Plug 'tomtom/tcomment_vim'

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
" personal prefs:
highlight! link Conceal Operator
" markdown headers: orange -> lime
highlight! link Directory Title
" voor Syntastic/gitgutter
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

" appearance
" ----------
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
set foldenable
set foldmethod=marker
set splitright

" statusline
" ----------
set laststatus=2
set ruler

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


" File-type specific
" ==================

" *unspecific
" -----------
autocmd BufNewFile,BufRead * :RainbowParentheses

" C
" -
autocmd BufNewFile,BufRead *.c,*.h set cindent

" Functions
" =========

" fill line with character(s)
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

" toggle ui elements 
" ----------------
let g:toggle_tabline=0
let g:hide_all=0

function! ToggleHideAll()
    if g:hide_all
        let g:toggle_tabline=0
        let g:hide_all=0
        set number
        set cc=80
        set showmode
        set ruler
        set laststatus=2
        set showcmd
        set showtabline=1
    else
        let g:toggle_tabline=1
        let g:hide_all=1
        set nonumber
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

" show underlines instead of highlighting
" ---------------------------------------
" function! UlSpell()
"     " highlight clear SpellBad
"     highlight SpellBad term=standout ctermfg=9 ctermbg=None term=underline cterm=underline
"     " highlight clear SpellCap
"     highlight SpellCap term=underline cterm=underline
"     " highlight clear SpellRare
"     highlight SpellRare term=underline cterm=underline
"     " highlight clear SpellLocal
"     highlight SpellLocal term=underline cterm=underline
" endfunction

" command! -nargs=0 Splln call UlSpell()

" create command aliases
" ----------------------
function! CommandAlias(key, value)
    exe printf('cabbrev <expr> %s (getcmdtype() == ":" && getcmdpos() <= %d) ? %s : %s', a:key, 1+len(a:key), string(a:value), string(a:key))
endfu

" 'run' mapping
" -----------
function! MapR()
    if (&ft=='pandoc')
        write
        !make
    elseif (&ft=='python')
        write
        !python %
    elseif (&ft=='c')
        write
        !make
    else
        write
    endif
endfunction

" command C -nargs=* call F ( <f-args> )

" Goyo callbacks
" --------------
function! s:goyo_enter()
    " Limelight
    colorscheme solarized
    set bg=light
endfunction

function! s:goyo_leave()
    " Limelight!
    colorscheme molokai
    set bg=dark
endfunction


" Plugins
" =======

" vim-airline
" -----------
let g:airline_detect_paste=1
let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts=1


" ctlp
" ----
" let g:ctrlp_working_path_mode='car'
" let g:ctrlp_switch_buffer='et'
" let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

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

" vim-wordy
" ---------


" Mappings
" ========

let mapleader = ","

" navigation
map j gj
map k gk

inoremap jj <esc>

" vnew
nnoremap <c-w><c-v> :vne<cr>

nmap <C-d> <C-w><C-h>
nmap <C-h> <C-w><C-j>
nmap <C-t> <C-w><C-k>
nmap <C-n> <C-w><C-l>

" replace
noremap <leader>gr :%s/\<<C-r><C-w>\>/
vnoremap <leader>gc y:%s/<C-r>"/
nnoremap <leader>lr :s/<C-r><C-w>/
vnoremap <leader>lr y:s/<C-r>"/

" copy pasta
nnoremap <leader>p "+p
vnoremap <leader>y "+y

" editing
nnoremap <leader>.p :tabedit $MYVIMRC<cr>
nnoremap <leader>eu :source $MYVIMRC<cr>
nnoremap <leader>tn :tabedit<cr>

" formatting
nnoremap Q vapgq
vnoremap Q gq

" word count
nnoremap <leader>wc :!wc -w % <bar> cut -d\  -f1<cr>

" 'run' mapping
nnoremap <leader>r :call MapR()<cr>

" hide statusline, linenumbers etc.
nnoremap <silent> <s-h> :call ToggleHideAll()<cr>
nnoremap <silent> <s-t> :call ToggleTabline()<cr>

" aliases
call CommandAlias("Q","q")
call CommandAlias("Wq","wq")
" vim-fugitive
call CommandAlias("W", "Gwrite")

" write as root
command! Sw :execute ':silent w !sudo tee % > /dev/null' | :edit! 
