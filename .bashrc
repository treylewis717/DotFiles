#
# ~/.bashrc
#

### EXPORT
export TERM="alacritty"
export EDITOR="nvim"
export PATH=/home/trey/.emacs.d/bin:/home/trey/.local/bin:$PATH

### ARCHIVE EXTRACTION
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;      
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

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
alias dfgit='/usr/bin/git --git-dir=/mnt/Games1/git-repos/DotFiles/ --work-tree=$HOME'
alias dfpush='dfgit push all'

# grep
alias grep='rg --color=auto'

# pacman and paru
alias sysupd="paru -Syu --noconfirm; sysclean" 	#Update Everything
alias sysclean='sudo paru -Rns $(paru -Qtdq)' #Remove orphaned packages

# Confirm Changes
alias cp="cp -iv"
alias mv='mv -iv'
alias rm='rm -iv'

# Clear
alias clear='clear; neofetch'

# Testing
alias testtetext='curl https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-demo.txt'

# Memey
alias rr='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

neofetch
eval "$(starship init bash)"

PS1='[\u@\h \W]\$ '
