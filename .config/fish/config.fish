### EXPORT
set fish_greeting
set TERM "alacritty"
set EDITOR "nvim"
set PATH "/home/trey/.local/bin:$PATH"
set LOCALBIN "/home/trey/.local/bin"

### ALIASES

# root
alias r='su -'

# ls
alias ls='exa -lah --color=always --group-directories-first'

# rm
alias frm="rm -rf"

# cat to bat
alias cat="bat"

# vim
alias vim="nvim"

# find
alias find="fd"

# Git DotFiles
alias dfgit='git --git-dir=/mnt/Games1/git-repos/DotFiles/ --work-tree=$HOME'
alias dfadd='dfgit add (bash dfadd)'
alias dfcommit="$LOCALBIN/dfcommit"
alias dfpush='dfgit push all'

# grep
alias grep='rg --color=auto'

# pacman and paru
alias sysupd="paru -Syu --noconfirm; sysclean" 	#Update Everything
alias sysclean='sudo paru -Rns (pacman -Qtdq)' #Remove orphaned packages

# Confirm Changes
alias cp="cp -iv"
alias mv='mv -iv'
alias rm='rm -iv'

# Clear
alias clear='clear; neofetch'

# Memey
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

### Startup
neofetch
starship init fish | source
