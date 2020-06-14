export SUDO_EDITOR=nvim

# bash-style backward kill
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# persistent rehash: https://wiki.archlinux.org/index.php/Zsh#Persistent_rehash
zstyle ':completion:*' rehash true

export PATH=~/.cargo/bin/:~/.local/bin:$PATH

alias rm='echo "This is not the command you are looking for."; false'

export PATH=~/.npm-packages/bin:$PATH

autoload -Uz vcs_info
precmd () { vcs_info }
setopt prompt_subst
PS1="\$vcs_info_msg_0_$PS1"

# IR AWS CLI Login: Begin ===== ===== =====
export AWS_CLI_LOGIN_SCRIPT_PATH="/home/vagrant/aws-cli-login/login.py"
export AWS_CLI_LOGIN_PYTHON_PATH="/home/vagrant/.local/share/virtualenvs/aws-cli-login-xmH2zWac/bin/python"
source "/home/vagrant/aws-cli-login/alias.sh"
# IR AWS CLI Login: End ===== ===== =====

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/vagrant/aristotle/dev-repos/categoriae/packages/resources/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/vagrant/aristotle/dev-repos/categoriae/packages/resources/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /home/vagrant/aristotle/dev-repos/categoriae/packages/resources/node_modules/tabtab/.completions/sls.zsh ]] && . /home/vagrant/aristotle/dev-repos/categoriae/packages/resources/node_modules/tabtab/.completions/sls.zsh
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /home/vagrant/aristotle/dev-repos/categoriae/packages/resources/node_modules/tabtab/.completions/slss.zsh ]] && . /home/vagrant/aristotle/dev-repos/categoriae/packages/resources/node_modules/tabtab/.completions/slss.zsh