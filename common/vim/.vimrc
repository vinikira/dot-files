"map leader
let mapleader="<space>"
"My settings
syntax on
let g:javascript_plugin_jsdoc = 1
set term=screen-256color
set cursorline
" Desabilitar backups:
set nobackup
set noswapfile
set nowritebackup
" Clipboard do sistema:
set clipboard=unnamed
" Tabs por espaços:
set expandtab
set shiftwidth=4
set tabstop=4
" Indentação:
filetype plugin indent on
set autoindent
" Régua, quebra e número de linhas:
set linebreak
set number
set relativenumber
set ruler
" auto complete tags
ino " ""<left>
ino ' ''<left>
ino ( ()<left>
ino [ []<left>
ino { {}<left>
ino {<CR> {<CR>}<ESC>O

" Navegação entre abas:
map <C-Tab> :tabnext<CR>
map <S-Tab> :tabprevious<CR>
" Salvar
nmap <silent> <C-S> :silent w<CR>
" Busca
set hlsearch
set ignorecase
set incsearch
" Limpar os resultados destacados:
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

"js beautify
autocmd FileType javascript vnoremap <buffer>  <c-f> :call RangeJsBeautify()<cr>
autocmd FileType json vnoremap <buffer> <c-f> :call RangeJsonBeautify()<cr>
autocmd FileType jsx vnoremap <buffer> <c-f> :call RangeJsxBeautify()<cr>
autocmd FileType html vnoremap <buffer> <c-f> :call RangeHtmlBeautify()<cr>
autocmd FileType css vnoremap <buffer> <c-f> :call RangeCSSBeautify()<cr>

if (has("termguicolors"))
    set termguicolors
endif

colorscheme dracula
