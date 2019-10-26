" ================================================================
" original
" ================================================================

set nobackup
set nocompatible

set number
set cursorcolumn
set cursorline

set tabstop=2
set backspace=2
set shiftwidth=2
set expandtab
set autoindent
set smartindent

set ignorecase
set incsearch

set ruler
set laststatus=2

syntax on
filetype plugin indent on

colo desert "to see all color schemes type :colo <C-d>

" disable stupid upper/lower case conversion
vmap u <nop>
vmap U <Nop>

nnoremap <PageUp>   :bprevious<CR>
nnoremap <PageDown> :bnext<CR>

:command E e
:command Q q
:command W w
:command Wq wq
:command WQ wq

