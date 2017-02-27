call plug#begin('~/.vim/autoload')
	Plug 'pangloss/vim-javascript' 
	Plug 'mxw/vim-jsx'
	Plug 'elzr/vim-json'
	Plug 'othree/jsdoc-syntax.vim'
	Plug 'heavenshell/vim-jsdoc'	
	Plug 'ternjs/tern_for_vim', { 'do': 'npm install' }
	Plug 'maksimr/vim-jsbeautify'
call plug#end()

filetype plugin indent on
syntax enable
