#
# ~/.bashrc
#

### EXPORT
export TERM="alacritty"
export EDITOR="nvim"

### ALIASES

# root
alias r='su -'

# ls
alias ls='exa -lah --color=always --group-directories-first'

# cat to bat
alias cat="bat"

# vim
alias vim="nvim"

# find
alias find="fd"

# git dotfiles
alias dfgit='/usr/bin/git --git-dir=/mnt/ODrive1/git-repos/DotFiles/ --work-tree=$HOME'
alias dfpush='dfgit push all'

# grep
alias grep='rg --color=auto'

# pacman and paru
alias sysupd="paru -Syu --noconfirm; rustup update; sysclean" 	#Update Everything
alias sysclean='sudo paru -Rns $(paru -Qtdq)' #Remove orphaned packages

# cp
alias cp="cp -iv"

# mv
alias mv='mv -iv'

# rm
alias rm='rm -iv'
alias frm="rm -rfv"

# wget
alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

# clear
alias clear='clear; echo " "; neofetch'

# diff
alias diff='colordiff'

# Testing
alias testtetext='curl https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-demo.txt'
alias testpl='ping archlinux.org'

# Memey
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

### Startup
echo " "
neofetch
eval "$(starship init bash)"

# PS1='[\u@\h \W]\$ '
source "$HOME/.cargo/env"
[ -f "/home/trey/.ghcup/env" ] && source "/home/trey/.ghcup/env" # ghcup-env
eval "$(zoxide init bash)"
