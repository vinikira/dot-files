" Plugins
call plug#begin()
Plug 'terryma/vim-multiple-cursors'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'joshdick/onedark.vim'
Plug 'sheerun/vim-polyglot'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'mattn/emmet-vim/'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'roxma/nvim-completion-manager'
Plug 'w0rp/ale'
Plug 'cohama/lexima.vim'
call plug#end()

" Nvim settings
syntax on
set encoding=utf-8
set background=dark
set cursorline
set mouse=a
set nobackup
set noswapfile
set nowritebackup
set clipboard=unnamedplus
set expandtab
set shiftwidth=4
set tabstop=4
filetype plugin indent on
set autoindent
set linebreak
set number
set relativenumber
set ruler
set hidden
set hlsearch
set ignorecase
set incsearch
set inccommand=split
colorscheme onedark
if (has("termguicolors"))
    set termguicolors
endif

" Keys
let mapleader="\<space>"
nmap <silent> <C-C> :silent noh<CR>
nmap <C-S> :w<CR>
nnoremap <c-p> :Files<cr>
nnoremap <c-f> :Ag<space>

" Ultisnips
let g:UltiSnipsSnippetsDir = '~/.config/nvim/UltiSnips'
let g:UltiSnipsEditSplit="vertical"

" jsdoc
let g:javascript_plugin_jsdoc = 1

" AirLine Configs
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
