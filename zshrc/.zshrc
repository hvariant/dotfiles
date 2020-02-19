export SUDO_EDITOR=nvim

# bash-style backward kill
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# persistent rehash: https://wiki.archlinux.org/index.php/Zsh#Persistent_rehash
zstyle ':completion:*' rehash true

export PATH=~/.cargo/bin/:~/.local/bin:$PATH

alias rm='echo "This is not the command you are looking for."; false'

export NVM_DIR="$HOME/.config/nvm"
if [ -d "$NVM_DIR" ]; then
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fi
