### EXPORT
set fish_greeting
set TERM "alacritty"
set EDITOR "nvim"


### ALIASES

# root
alias r='su -'

# ls
alias ls='exa -al --color=always --group-directories-first'

# rm
alias frm="rm -rf"

# vim
alias vim="nvim"

# Git DotFiles
alias dfgit='/usr/bin/git --git-dir=/mnt/Games1/git-repos/DotFiles/ --work-tree=$HOME'

# grep
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# pacman and yay
alias sysupd="yay -Syu --noconfirm" 		  #Update Everything
alias sysclean='sudo pacman -Rns $(pacman -Qtdq)' #Remove orphaned packages

# Confirm Changes
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# Clear
alias clear='clear; neofetch'

# Memey
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

### Startup
neofetch
starship init fish | source
