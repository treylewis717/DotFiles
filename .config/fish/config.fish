### EXPORT

# Disables greeting
set fish_greeting

# Sets $TERM as alacritty
set TERM "alacritty"

# Sets my editor to Neovim
set EDITOR "nvim"

# Zoxide will echo the matched dir before navigating
set -gx _ZO_ECHO 1

# Sets my chrome executable
set -gx CHROME_EXECUTABLE "/usr/bin/google-chrome-stable"

# Sets the Android SDK location
set -gx ANDROID_SDK_ROOT "/home/trey/Android/Sdk"

# Enables ghcup in shell
[ -f "/home/trey/.ghcup/env" ] && source "/home/trey/.ghcup/env" # ghcup-env

### ALIASES

# ls
alias ls='exa -lah --icons --color=always --group-directories-first --git --binary'

# cat to bat
alias cat="bat"

# vim
alias vim="nvim"

# find
alias find="fd"

# git dotfiles
alias dfgit='/usr/bin/git --git-dir=/home/trey/Projects/Git/DotFiles/ --work-tree=$HOME'
alias dfpush='dfgit push'

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

# wget
alias wget='wget --hsts-file="$XDG_DATA_HOME/wget-hsts"'

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

echo " "
neofetch
starship init fish | source
zoxide init --cmd cd fish | source
