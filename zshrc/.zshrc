export SUDO_EDITOR=nvim

# bash-style backward kill
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# persistent rehash: https://wiki.archlinux.org/index.php/Zsh#Persistent_rehash
zstyle ':completion:*' rehash true

export PATH=~/.cargo/bin/:~/.local/bin:$PATH

alias rm='echo "This is not the command you are looking for."; false'

export PATH=~/.npm-packages/bin:$PATH



# IR AWS CLI Login: Begin ===== ===== =====
export AWS_CLI_LOGIN_SCRIPT_PATH="/home/vagrant/aws-cli-login/login.py"
export AWS_CLI_LOGIN_PYTHON_PATH="/home/vagrant/.local/share/virtualenvs/aws-cli-login-xmH2zWac/bin/python"
source "/home/vagrant/aws-cli-login/alias.sh"
# IR AWS CLI Login: End ===== ===== =====
