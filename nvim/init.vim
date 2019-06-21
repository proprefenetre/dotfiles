" vimrc -- Sinterklaas 2018 iteration 
" For terminal use only (though not for murder)

" General
" =======
set nocompatible
filetype plugin indent on
syntax enable

if (has("termguicolors"))
     set termguicolors
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
set textwidth=119
set cc=+1               
set fo+=j
set splitright

" folds
" -----
set foldenable
set foldmethod=manual
set foldnestmax=10

" statusline
" ----------
set laststatus=2
"set statusline=
"set statusline+=%m\ 
"set statusline+=%<%f\ 
"set statusline+=[%(%r\,\ %)%y\,\ %{(&fenc!=''?&fenc:&enc)}]
"set statusline+=\ %=%-14.(%l,%c%v%)\ %p

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

" general style
" -------------
set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

set formatprg=par\ -w80q

" swp/backup files
" ================
set backupdir=~/.config/nvim/backup//,/tmp//
set directory=~/.config/nvim/swap//,/tmp//
set undodir=./.config/nvim/undo//,/tmp//

" plugins
" =======
if empty(glob('/Users/niels/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo /Users/niels/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/plugged')
Plug 'jacoborus/tender.vim'
Plug 'tomasr/molokai'
Plug 'joshdick/onedark.vim'

Plug 'tpope/vim-eunuch' " SudoWrite &c
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/rainbow_parentheses.vim'

Plug 'sheerun/vim-polyglot'

Plug 'sirver/ultisnips'
 
call plug#end()

" plugin configs
" ==============

colorscheme tender

" Rainbow Parentheses
" -------------------
autocmd BufNewFile,BufRead * :RainbowParentheses
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}'], ['<', '>']]

" functions
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
" ------------------
let g:toggle_tabline=0
let g:hide_all=0

function! ToggleHideAll()
    if g:hide_all
        let g:toggle_tabline=0
        let g:hide_all=0
        set number
        hi linenr guifg=#bcbcbc
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
        set nonumber
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

" create command aliases 
" ----------------------
function! CommandAlias(key, value)
    exe printf('cabbrev <expr> %s (getcmdtype() == ":" && getcmdpos() <= %d) ? %s : %s', a:key, 1+len(a:key), string(a:value), string(a:key))
endfu

" 'run' mapping 
" -------------
function! MapR()
    if (&ft=='rust')
        write
        !cargo run
    elseif (&ft=='pandoc')
        write
        make
    elseif (&ft=='python')
        write
        !python3 %
    elseif (&ft=='c')
        write
        make
    elseif (&ft=='yaml')
        !yamllint %
    endif
endfunction

" Mappings
" ========

let mapleader = ","

" navigation
map j gj
map k gk

nnoremap <leader>q :wq<cr>

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
"
"Remove all trailing whitespace
nnoremap <leader>1 :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

" word count
nnoremap <leader>2 :!wc -w % <bar> cut -d\  -f1<cr>

" formatting
nnoremap <leader>3 vapgq
vnoremap <leader>3 gq

" copy pasta
nnoremap <leader>p "+p
vnoremap <leader>y "+y

" editing vimrc
nnoremap <leader>i :e $MYVIMRC<cr>
nnoremap <C-c>r :source $MYVIMRC<cr>

" 'run' mapping
nnoremap <leader>r :call MapR()<cr>

" hide statusline, linenumbers etc.
nnoremap <silent> <leader><s-h> :call ToggleHideAll()<cr>
nnoremap <silent> <leader><s-t> :call ToggleTabline()<cr>

" command line keys
cnoremap <c-a> <Home>

" aliases
call CommandAlias("reindent", ":!indent -kr %")
call CommandAlias("W", "w")
call CommandAlias("Q","q")
call CommandAlias("Wq","wq")
call CommandAlias("Sx", ":SudoWrite<cr> :q<cr>")
