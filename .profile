# Set WINEPREFIX
export WINEPREFIX="$XDG_CONFIG_HOME"/wineprefix

# Set XDG directories
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CACHE_HOME="$HOME/.cache"

# $HOME Cleanup
export HISTFILE="$XDG_STATE_HOME"/bash/history # ~/.bash_history
export CABAL_CONFIG="$XDG_CONFIG_HOME"/cabal/config # ~/.cabal
export CABAL_DIR="$XDG_DATA_HOME"/cabal # ~/.cabal
export CARGO_HOME="$XDG_DATA_HOME"/cargo # ~/.cargo
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv # ~/.nv
export GNUPGHOME="$XDG_DATA_HOME"/gnupg # ~/.gnupg
export GRADLE_USER_HOME="$XDG_DATA_HOME"/gradle # ~/.gradle
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc # ~/.gtkrc-2.0
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history # ~/.lesshst
export MYPY_CACHE_DIR="$XDG_CACHE_HOME"/mypy # ~/.mypy_cache
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java # ~/.java
export PYTHONSTARTUP="$XDG_CONFIG_HOME"/python/pythonrc # ~/.python_history
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup # ~/.rustup
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
export STACK_ROOT="$XDG_DATA_HOME"/stack

# PATH
export PATH="$HOME"/.ghcup/bin/:"$HOME"/Qt/Tools/QtCreator/bin/:"$HOME"/android-platform-tools:"$CARGO_HOME"/bin/:"$XDG_CONFIG_HOME"/emacs/bin:"$HOME"/.local/bin:"$HOME"/Programming/SDKs/flutter/bin:"$HOME"/MultiMC/bin:"$HOME"/Applications:"$XDG_DATA_HOME"/applications/wine/Programs/:$PATH
