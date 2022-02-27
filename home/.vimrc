set nocompatible  " avoid vi compatibility


if has('filetype')
   filetype indent plugin on
endif

" Reload with :source ~/.vimrc
" use jk as escape
inoremap jk <ESC>

let mapleader = "'"

syntax on " highlight syntax
set number " show line numbers
set noswapfile " disable the swapfile
set hlsearch " highlight all results
set ignorecase " ignore case in search
set incsearch " show search results as you type

set cursorline

set expandtab
set autoindent
set tabstop=4

set hidden

if has('autochdir1')
   set autochdir
endif

" Quick buffer switching
nnoremap <leader>' :ls<CR>:b<Space>
" :help buffers
