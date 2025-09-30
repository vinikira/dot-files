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
set shiftwidth=4
set tabstop=4
filetype plugin indent on
set autoindent
set linebreak
set number
set relativenumber
set ruler
set mouse=a
set encoding=utf-8
set hidden
set cmdheight=2
set updatetime=300
set shortmess+=c

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

if (has("termguicolors"))
    set termguicolors
endif

colorscheme desert 
