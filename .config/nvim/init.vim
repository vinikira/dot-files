"map leader
let mapleader=","
"My settings
syntax on
let g:javascript_plugin_jsdoc = 1
set foldmethod=syntax
set background=dark
set cursorline
""set term=screen-256color
" Desabilitar backups:
set nobackup
set noswapfile
set nowritebackup
" Clipboard do sistema:
set clipboard=unnamedplus
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
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')

Plug 'pangloss/vim-javascript'
Plug 'scrooloose/nerdtree'
Plug 'bling/vim-airline'
Plug 'scrooloose/nerdcommenter'
Plug 'altercation/vim-colors-solarized'
"Plug 'valloric/youcompleteme'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'carlitux/deoplete-ternjs', { 'do': 'sudo npm install -g tern' }
Plug 'tpope/vim-surround'
Plug 'joshdick/onedark.vim'
Plug 'sheerun/vim-polyglot'
Plug 'maksimr/vim-jsbeautify'
Plug 'vim-syntastic/syntastic'
Plug 'kien/ctrlp.vim'

call plug#end()


"-------------------------------------------------------------

let g:deoplete#enable_at_startup = 1

"NerdTree configs--------------------------------------------
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif

autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif

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

"syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1

"ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'

"ternjs
 let g:tern_request_timeout = 1
 let g:tern_show_signature_in_pum = '0'  " This do disable full signature

 "Add extra filetypes
 let g:tern#filetypes = [
                 \ 'jsx',
                 \ 'javascript.jsx',
                 \ 'vue']



if (has("termguicolors"))
    set termguicolors
endif

colorscheme onedark
