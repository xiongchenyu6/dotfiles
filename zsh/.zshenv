if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export JAVA_HOME='/usr/lib/jvm/java-8-openjdk'
    export ANDROID_HOME='/opt/android-sdk'
    export XAUTHORITY=~/.Xauthority
    # export PATH=/opt/ruby2.4/bin:$PATH
    # ...
else [[ "$OSTYPE" == "darwin"* ]];
    # Mac OSX
    export ANDROID_HOME='/usr/local/share/android-sdk'
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export SCALA_HOME=/usr/local/opt/scala/idea
    export JDK_HOME="$(/usr/libexec/java_home)"
    export PATH=/usr/local/sbin:$PATH
    export PATH=/usr/local/bin:$PATH
    export PATH=/usr/local/opt/ruby/bin:$PATH
    export PATH=/usr/local/lib/ruby/gems/2.6.0/bin:$PATH
    export PATH=$HOME/.nix-profile/bin/:$PATH
    export PKG_CONFIG_PATH="/usr/local/opt/libxml2/lib/pkgconfig"
fi

export GOPATH=$HOME/go
export PATH=$HOME/.local/bin/:$PATH
export PATH=$HOME/.yarn/bin/:$PATH
export PATH=$HOME/.emacs.d/bin/:$PATH

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"
export LC_ALL=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=xterm-256color
export HISTCONTROL=ignoreboth:erasedups

export GHTOKEN="ad38f8a815e974c98db2abd6f5ff304eca53400f"
# alias cat=ccat
alias vim='nvim'
alias git=hub
alias yolo='git commit -m "$(curl -s whatthecommit.com/index.txt)"'
alias ensime="gtags & sbt clean ensimeConfig test:compile ensimeServerIndex"

export NIX_PATH=$NIX_PATH:$HOME/.nix-defexpr/channels
export SBT_OPTS="-Xmx8G"
export EDITOR="emacsclient"
export EMAIL="xiongchenyu@bigo.sg"


export NODE_PATH=$HOME/.config/yarn/global/node_modules

export CONAN_USER_HOME=/home/chenyu  # 设置你的缓存地址，开发机上面请不要放到home
export CONAN_USERNAME=brec
export CONAN_CHANNEL=stable

export XMODIFIERS=@im=ibus

#CJK index
export XAPIAN_CJK_NGRAM=1

#export CC=gcc
#export CXX=g++

#export CC=gcc-5
#export CXX=g++-5

export CC=clang
export CXX=clang++
export MAKEFLAGS="-j8"

export INSECURE=1
#
#GTAGS
export GTAGSLABEL=pygments
export GTAGSTHROUGH=true

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing nonexistent history.
