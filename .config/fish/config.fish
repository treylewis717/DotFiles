### EXPORT
set fish_greeting
set TERM "alacritty"
set EDITOR "nvim"
set -gx RUST_BACKTRACE 1
set -gx CHROME_EXECUTABLE "/usr/bin/google-chrome-stable"
[ -f "/home/trey/.ghcup/env" ] && source "/home/trey/.ghcup/env" # ghcup-env

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
alias sysclean='paru -Rns (paru -Qtdq)' #Remove orphaned packages

# cp
alias cp="cp -iv"

# mv
alias mv='mv -iv'

# rm
alias rm='rm -iv'
alias frm="rm -rfv"

# lightdm
alias lock='light-locker-command -l'

# clear
alias clear='/bin/clear; echo " "; neofetch'

# diff
alias diff='colordiff'

# testing
alias testtetext='curl https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-demo.txt'
alias testpl='ping archlinux.org'

# memey
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

### Startup
# scd fish-init | source
echo " "
neofetch
starship init fish | source
