" vimrc -- January 2019 
" For terminal use only (though not for murder)

" Preamble
" ========
set nocompatible
filetype plugin indent on
syntax enable

set backspace=indent,eol,start
set mouse=a

set t_Co=256

" plugins
" =======

if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

Plug 'junegunn/fzf.vim'
Plug '/usr/bin/fzf'
" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

Plug 'junegunn/rainbow_parentheses.vim'

Plug 'sheerun/vim-polyglot'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-repeat'

Plug 'jiangmiao/auto-pairs'

Plug 'unblevable/quick-scope'
let g:qs_enable=0

" Plug 'sirver/ultisnips'

" Plug 'dense-analysis/ale'
" let g:ale_linters = {'python': ['flake8']}

Plug 'natebosch/vim-lsc'
let g:lsc_server_commands = {'python': 'pyls'}
" Use all the defaults (recommended):
let g:lsc_auto_map = v:true

Plug 'yggdroot/indentline'

Plug 'tomasr/molokai'

Plug 'sjl/badwolf/'

Plug 'easymotion/vim-easymotion' 
let g:EasyMotion_smartcase = 1

Plug 'justinmk/vim-dirvish'
call plug#end()

" General
" =======

colorscheme molokai
let g:rehash256 = 1

highlight clear LineNr
highlight clear SignColumn

set encoding=utf-8
set hidden
set number
set showcmd
set wildmenu
set wildmode=longest:list,full
set concealcursor=c
set showmatch
set scrolloff=999
set textwidth=80
set cc=+1               
set fo+=j
set splitright

" folds
" -----
set foldmethod=manual
set foldnestmax=10

" statusline
" ----------
set laststatus=2
set statusline=
set statusline+=%M\ 
set statusline+=%<%f\ 
set statusline+=[%(%R\,\ %)%Y\,\ %{(&fenc!=''?&fenc:&enc)}]
set statusline+=\ %=%-14.(%l,%c%V%)\ %P

" auto-anything
" -------------
set noautochdir
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
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab

" set formatprg=par\ -w80q

" swp/backup files
" ================
set backupdir=$HOME/.vim/backup//
set directory=$HOME/.vim/swap//
set undodir=$HOME/.vim/undo//

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
" ------------------
let g:toggle_tabline=0
let g:hide_all=0

function! ToggleHideAll()
    if g:hide_all
        let g:toggle_tabline=0
        let g:hide_all=0
        set number
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

let mapleader = ";"

nnoremap , ;

" navigation
map j gj
map k gk

nnoremap <C-a> 0
nnoremap <C-e> $

inoremap jj <esc>

" Easymotion
nmap s <Plug>(easymotion-s)

"" JK motions: Line motions
map <leader>j <Plug>(easymotion-j)
map <leader>k <Plug>(easymotion-k)

"" command line keys
cnoremap <c-a> <Home>

nnoremap <c-x>f :Dirvish<cr>
nnoremap <c-x>b :Buffers<cr>

" windows 'n splits
nnoremap <c-x>3 :vne<cr>
nnoremap <c-x>2 :new<cr>
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

" copy pasta
nnoremap <leader>p "+p
vnoremap <leader>y "+y

" editing vimrc
nnoremap <leader>i :e $MYVIMRC<cr>
" nnoremap <leader>r :source $MYVIMRC<cr>

" hide statusline, linenumbers etc.
nnoremap <silent> <leader><s-h> :call ToggleHideAll()<cr>
nnoremap <silent> <leader><s-t> :call ToggleTabline()<cr>

" aliases
call CommandAlias("reindent", ":!indent -kr %")
call CommandAlias("W", "w")
call CommandAlias("Q","q")
call CommandAlias("Wq","wq")
call CommandAlias("Sx", ":SudoWrite<cr> :q<cr>")

nnoremap <leader>q :q<cr>

" write as root
command! Sw :SudoWrite
command! Se :SudoEdit
