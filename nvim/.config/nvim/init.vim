call plug#begin('~/.local/share/nvim/plugged')

" <leader>c to comment/uncomment
Plug 'scrooloose/nerdcommenter'

call plug#end()

set nobackup

set autoread
au FocusGained * :checktime

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

colo pablo

" disable uppercase/lowercase conversion with U/u
vmap u <nop>
vmap U <Nop>

" navigating buffers
nnoremap <PageUp>   :bprevious<CR>
nnoremap <PageDown> :bnext<CR>

" navigating tabs
nnoremap <S-PageUp>   :tabnext<CR>
nnoremap <S-PageDown> :tabprevious<CR>

" terminal mode escape
tnoremap <Esc> <C-\><C-n>

" common typos
:command E e
:command Q q
:command W w
:command Wq wq
:command WQ wq

" https://stackoverflow.com/q/1675688
set list