" Map leader
let mapleader="<space>"

" My settings
syntax on
set cursorline
set nobackup
set noswapfile
set nowritebackup
set clipboard=unnamed
set expandtab
set shiftwidth=2
set tabstop=2
filetype plugin indent on
set autoindent
set linebreak
set number
set relativenumber
set ruler
set mouse=a

" Pair parens
ino " ""<left>
ino ' ''<left>
ino ( ()<left>
ino [ []<left>
ino { {}<left>
ino {<CR> {<CR>}<ESC>O

" Tabs navigation
map <C-Tab> :tabnext<CR>
map <S-Tab> :tabprevious<CR>

" Save with leader
nmap <silent> <C-S> :silent w<CR>

" Search settings
set hlsearch
set ignorecase
set incsearch

" Clean highlighted search results 
nmap <silent> <C-C> :silent noh<CR>

"Plugs------------------------------------------------------
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'bling/vim-airline'
Plug 'scrooloose/nerdcommenter'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'sheerun/vim-polyglot'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'elixir-editors/vim-elixir'
call plug#end()

"-------------------------------------------------------------

" NerdTree configs--------------------------------------------
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

map <C-n> :NERDTreeToggle<CR>
"------------------------------------------------------------

" AirLine Configs--------------------------------------------
let g:airline#extensions#tabline#enabled = 1
"------------------------------------------------------------

if (has("termguicolors"))
    set termguicolors
endif

colorscheme dracula
