" START LOCAL CUSTOMIZATION
" END LOCAL CUSTOMIZATION

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
set smartcase " ignore case search, unless a cap is used
" set ignorecase " ignore case in search
set incsearch " show search results as you type

set cursorline

set expandtab
set autoindent
set tabstop=4

set hidden

if has('autochdir')
   set autochdir
endif

" Quick buffer switching
nnoremap <leader>' :ls<CR>:b<Space>
" :help buffers

" for python
set shiftwidth=4
set softtabstop=4

" get menus of completion for ex commands
" set wildmenu
" set wildmode=full

" get menus of completion for ex commands
set wildmode=longest,list


" command history
set history=200

" from unimpaired plugin via practical vim
nnoremap <silent> [b :bprevious<CR>
nnoremap <silent> ]b :bnext<CR>
nnoremap <silent> ]B :bfirst<CR>
nnoremap <silent> ]B :blast<CR>

" practical vim, vimcasts.org/episodes/the-edit-command
" expand %% in command-line prompt to path of active buffer
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

if has("autocmd")
   filetype on
   autocmd FileType python setlocal ts=4 sts=4 sw=4
endif

" START LOCAL CUSTOMIZATION
" END LOCAL CUSTOMIZATION
