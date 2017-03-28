"My settings
syntax on
set number
let g:javascript_plugin_jsdoc = 1
set foldmethod=syntax
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
set background=dark
set cursorline

"Plugins------------------------------------------------------
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'scrooloose/nerdtree'
Plugin 'bling/vim-airline'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-fugitive'
Plugin 'altercation/vim-colors-solarized'
Plugin 'valloric/youcompleteme'


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

"-------------------------------------------------------------

"NerdTree configs--------------------------------------------
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

map <C-n> :NERDTreeToggle<CR>
"------------------------------------------------------------

"AirLine Configs---------------------------------------------
let g:airline#extensions#tabline#enabled = 1
"------------------------------------------------------------

"Tagbar Configs----------------------------------------------
nmap <F8> :TagbarToggle<CR>
"------------------------------------------------------------

"ColorScheme Configs----------------------------------------
let g:solarized_termcolors=256
colorscheme solarized
"-----------------------------------------------------------


