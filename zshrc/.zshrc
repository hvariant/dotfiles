export SUDO_EDITOR=nvim

# bash-style backward kill
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# persistent rehash: https://wiki.archlinux.org/index.php/Zsh#Persistent_rehash
zstyle ':completion:*' rehash true

export PATH=~/.cargo/bin/:~/.local/bin:$PATH

alias rm='echo "This is not the command you are looking for."; false'

autoload -Uz vcs_info
precmd () { vcs_info }
setopt prompt_subst
PS1="\$vcs_info_msg_0_$PS1"
