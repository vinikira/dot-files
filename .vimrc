"My settings
syntax on
let g:javascript_plugin_jsdoc = 1
set foldmethod=syntax
set background=dark
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

call plug#begin('~/.vim/plugged')

Plug 'pangloss/vim-javascript'
Plug 'scrooloose/nerdtree'
Plug 'bling/vim-airline'
Plug 'scrooloose/nerdcommenter'
Plug 'altercation/vim-colors-solarized'
Plug 'valloric/youcompleteme'
Plug 'joshdick/onedark.vim'
Plug 'sheerun/vim-polyglot'

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

if (has("termguicolors"))
    set termguicolors
endif

colorscheme onedark
