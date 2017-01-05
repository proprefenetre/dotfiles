" vimrc -- Sinterklaas iteration

" Preamble
" ========

set nocompatible
filetype plugin indent on
syntax enable

call plug#begin('~/.vim/plugged')

Plug 'sirver/ultisnips'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tomasr/molokai'
Plug 'altercation/vim-colors-solarized'

Plug 'scrooloose/nerdtree'

Plug 'vim-syntastic/syntastic'

Plug 'xolox/vim-misc'
Plug 'xolox/vim-easytags'
Plug 'majutsushi/tagbar'

Plug 'ctrlpvim/ctrlp.vim'
Plug 'vim-scripts/a.vim'

Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
" Plug 'tpope/vim-commentary'
Plug 'tomtom/tcomment_vim'

Plug 'christoomey/vim-tmux-navigator'
Plug 'godlygeek/tabular'

Plug 'junegunn/limelight.vim'
Plug 'junegunn/goyo.vim'

Plug 'Raimondi/delimitMate'
Plug 'junegunn/rainbow_parentheses.vim'

call plug#end()

" General
" =======

let g:molokai_original = 1
let g:rehash256 = 1
colorscheme molokai

if has ("gui_running")
	set guifont=Hack\ 10
	set go-=e
	set go-=m
	set go-=r
	set go-=L
	set go-=T
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
set conceallevel=2
set concealcursor=c
set showmatch
set scrolloff=999
set textwidth=79
set cc=+1               
set fo+=j

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

hi clear SignColumn	" voor Syntastic/gitgutter

" general style
" -------------
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab


" File type specific
" ==================

" *unspecific
" -----------
autocmd BufNewFile,BufRead * :RainbowParentheses
autocmd BufNewFile,BufRead * call UlSpell()

" python
" ------
autocmd BufNewFile,BufRead *.py call MapR()

" C
" -
autocmd BufNewFile,BufRead *.c,*.h set cindent
	    \| call MapR()

" markdown
" --------
autocmd BufNewFile,BufFilePre,BufRead *.md set filetype=markdown
	    \| call Prose()
	    \| call MapR()

" mail
" ----
autocmd FileType mail call Prose()


" Functions
" =========

" show underlines instead of highlighting
" ---------------------------------------
function! UlSpell()
	" highlight clear SpellBad
	highlight SpellBad term=standout ctermfg=9 ctermbg=None term=underline cterm=underline
	" highlight clear SpellCap
	highlight SpellCap term=underline cterm=underline
	" highlight clear SpellRare
	highlight SpellRare term=underline cterm=underline
	" highlight clear SpellLocal
	highlight SpellLocal term=underline cterm=underline
endfunction


" create command aliases
" ----------------------
function! CommandAlias(key, value)
	exe printf('cabbrev <expr> %s (getcmdtype() == ":" && getcmdpos() <= %d) ? %s : %s', a:key, 1+len(a:key), string(a:value), string(a:key))
endfu

" a writing mode/environment
" --------------------------
function! Prose()
	" keep everything as local as possible
	setlocal spell
	if has ('gui_running')
		colorscheme solarized
		setlocal bg=light
	endif
	" use the first spelling correction
	nnoremap <buffer> <leader>s 1z=
	inoremap <buffer> <c-s> <c-g>u<Esc>1z=<c-g>u

	" formatting shortcuts
	nnoremap <buffer> <silent> Q vapgq
	xnoremap <buffer> <silent> Q gq
	nnoremap <buffer> <silent> <leader>Q vapjgqap
endfunction

command! -nargs=0 Prose call Prose()

" run mapping
" -----------
function! MapR()
	if (&ft=='markdown')
		nnoremap <leader>r :w<cr>:!make<cr>
	elseif (&ft=='python')
		nnoremap <leader>r :w<cr>:!python %<cr>
	elseif (&ft=='c')
		nnoremap <leader>r :w<cr>:!make<cr>
	endif
endfunction


" Goyo callbacks
" --------------
function! s:goyo_enter()
	Limelight
	syntax on
endfunction

function! s:goyo_leave()
	Limelight!
	syntax on
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


" Goyo
" ----
autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()


" Limelight
" ---------
let g:limelight_conceal_ctermfg = 'gray'
let g:limelight_conceal_ctermfg = 240
let g:limelight_default_coefficient = 0.4
let g:limelight_paragraph_span = 1
let g:limelight_priority = -1


" UltiSnips
" ---------
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsSnippetDirectories=["UltiSnips"]
let g:UltiSnipsEditSplit="vertical"


" Rainbow parentheses
" -------------------
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']'], ['{', '}']]


" NERDTree
" --------
map <F1> :NERDTreeToggle<cr>
let g:NERDTreeWinPos = "right"
" close vim if last window left is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif


" vim-gitgutter
" -------------
let g:airline#extensions#hunks#non_zero_only = 1


" vim-easytags
" ------------
set tags=./tags;,~/.vimtags
let g:easytags_events = ['BufReadPost', 'BufWritePost']
let g:easytags_async = 1
let g:easytags_dynamic_files = 2
let g:easytags_resolve_links = 1
let g:easytags_suppress_ctags_warning = 1


" vim-tagbar
" ----------
nmap <leader>b :TagbarToggle<CR>


" vim-delimitMate
" ---------------
let delimitMate_expand_cr = 1
augroup mydelimitMate
	au!
	au FileType markdown let b:delimitMate_nesting_quotes = ["`"]
	au FileType tex let b:delimitMate_quotes = ""
	au FileType tex let b:delimitMate_matchpairs = "(:),[:],{:},`:'"
	au FileType python let b:delimitMate_nesting_quotes = ['"', "'"]
augroup END


" Mappings
" ========

let mapleader = ","

" general 
nnoremap <leader>g :Goyo<cr>
" navigation
map j gj
map k gk

inoremap jj <esc>

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
nnoremap Q gq

" word count
nnoremap <leader>wc :!wc -w % <bar> cut -d\  -f1<cr>

" :W = :w
call CommandAlias("W","w")
call CommandAlias("Q","q")

" write as root
command! Sw :execute ':silent w !sudo tee % > /dev/null' | :edit! 
