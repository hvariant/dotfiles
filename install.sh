#!/bin/sh

stow i3 -t ~
stow nvim -t ~
stow vim -t ~
stow zshrc -t ~
stow gitconfig -t ~

curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
nvim +PlugInstall +qa
