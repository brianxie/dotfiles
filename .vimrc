set nocompatible

filetype plugin indent on

" Editing
set ignorecase

" Display
set background=dark
set hlsearch
set incsearch
set laststatus=2
set noshowmode
set number
set ruler
set showmatch
set visualbell
syntax enable

" Indentation
set autoindent
set expandtab
set shiftwidth=2
set smarttab
set softtabstop=2
set tabstop=2

" Packages
packadd! onedark.vim
colorscheme onedark

packadd! lightline.vim
let g:lightline = { 'colorscheme': 'one' }
