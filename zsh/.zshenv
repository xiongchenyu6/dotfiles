if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export JAVA_HOME='/usr/lib/jvm/java-13-openjdk'
    export HADOOP_HOME=/usr/lib/hadoop
    export CONFLUENT_HOME=/
    export FLINK_HOME=/opt/flink-1.10.0
    export SPARK_HOME=/opt/spark-2.4.3
    export HBASE_HOME=/opt/hbase-2.2.0
    export HIVE_HOME=/opt/hive-3.1.1
    export ASYNC_PROFILER_HOME=/opt/async-profiler
    export XAUTHORITY=~/.Xauthority
    export CONAN_USERNAME=brec
    export CONAN_CHANNEL=stable
    export HADOOP_CLASSPATH=`hadoop classpath`:$HADOOP_HOME/lib/native/*
    export SPARK_DIST_CLASSPATH=`hadoop classpath`:$HBASE_HOME/lib/hbase-common-2.1.5.jar:$HBASE_HOME/lib/hbase-client-2.1.5.jar:$HBASE_HOME/lib/hbase-mapreduce-2.1.5.jar:$HBASE_HOME/lib/hbase-shaded-miscellaneous-2.1.0.jar:$HBASE_HOME/lib/hbase-shaded-protobuf-2.1.0.jar:$HBASE_HOME/lib/hbase-shaded-netty-2.1.0.jar
    export BUILD_DIR=/tmp/rrdbuild
    export GEM_HOME=$HOME/.gem/ruby/2.7.0

else [[ "$OSTYPE" == "darwin"* ]];
    export ANDROID_HOME=/usr/local/share/android-sdk
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export SCALA_HOME=/usr/local/opt/scala/idea
    export JDK_HOME="$(/usr/libexec/java_home)"
    export RUBY_HOME=/usr/local/opt/ruby
    export GEM_HOME=/usr/local/lib/ruby/gems/2.6.0
    export PATH=/usr/local/sbin:$PATH
    export PATH=/usr/local/bin:$PATH
    export PKG_CONFIG_PATH="/usr/local/opt/libxml2/lib/pkgconfig"
    export CONAN_USER_HOME=/Users/xiongchenyu
fi

export PATH=$HOME/.nix-profile/bin/:$PATH
export PATH=$HOME/.node_modules/bin:$PATH
export PATH=$HOME/.script:$PATH

export HADOOP_INSTALL=$HADOOP_HOME
export HADOOP_MAPRED_HOME=$HADOOP_HOME 
export HADOOP_COMMON_HOME=$HADOOP_HOME 
export HADOOP_HDFS_HOME=$HADOOP_HOME 
export YARN_HOME=$HADOOP_HOME 
export HADOOP_COMMON_LIB_NATIVE_DIR=$HADOOP_HOME/lib/native 
export HADOOP_OPTS="$HADOOP_OPTS -Djava.library.path=$HADOOP_HOME/lib/native"
export JAVA_LIBRARY_PATH=$JAVA_LIBRARY_PATH:$HADOOP_HOME/lib/native
export FLINK_CLASSPATH=$HADOOP_HOME/lib/native/*
export CC_CLASSPATH=$HADOOP_HOME/lib/native/*


#hadoop echo
export PATH=$HADOOP_HOME/sbin:$HADOOP_HOME/bin:$PATH
export PATH=$FLINK_HOME/bin:$PATH
export PATH=$HBASE_HOME/bin:$PATH
export PATH=$SPARK_HOME/bin:$PATH
export PATH=$HIVE_HOME/bin:$PATH
export PATH=$RUBY_HOME/bin:$PATH
export PATH=$GEM_HOME/bin:$PATH
export PATH=$ASYNC_PROFILER_HOME:$PATH


export npm_config_prefix=~/.node_modules

export METALS_ENABLED=true

export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH
export PATH=$HOME/.local/bin/:$PATH
export PATH=$HOME/.yarn/bin/:$PATH
export PATH=$HOME/.emacs.d/bin/:$PATH
export HADOOP_LOG_DIR=/data/logs

 #ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"
export LC_ALL=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=xterm-256color
export HISTCONTROL=ignoreboth:erasedups

export GHTOKEN="ad38f8a815e974c98db2abd6f5ff304eca53400f"
#  alias cat=ccat
alias vi='nvim'
alias git=hub
alias yolo='git commit -m "$(curl -s whatthecommit.com/index.txt)"'

export NIX_PATH=$NIX_PATH:$HOME/.nix-defexpr/channels
export SBT_OPTS="-Xss2M"
export EDITOR="emacsclient"
export EMAIL="xiongchenyu@bigo.sg"


export NODE_PATH=$HOME/.config/yarn/global/node_modules

export XMODIFIERS=@im=ibus

#CJK index
export XAPIAN_CJK_NGRAM=1

export CC=clang
export CXX=clang++

#input method
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export XIM=ibus
export QT_IM_MODULE=ibus
export XIM_ARGS="ibus-daemon -d -x"

#
export MAKEFLAGS="-j8"

export INSECURE=1
export FZF_TMUX=1

#GTAGS
export GTAGSLABEL=pygments
export GTAGSTHROUGH=true
export SYSTEMD_DEBUGGER=gdb

#Theme
export POWERLEVEL9K_MODE='nerdfont-complete'	
export POWERLEVEL9K_VCS_SHOW_SUBMODULE_DIRTY=true	# set the theme to something, or blank if you use a non omz theme
export POWERLEVEL9K_DIR_OMIT_FIRST_CHARACTER=true
export POWERLEVEL9K_SHOW_CHANGESET=true	
export POWERLEVEL9K_PROMPT_ON_NEWLINE=true
export POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(context root_indicator disk_usage dir dir_writable dropbox virtualenv root_indicator pyenv vcs)	
export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs command_execution_time history time newline)

export SPACESHIP_GIT_BRANCH_SHOW_COMMIT=true
export SPACESHIP_GIT_BRANCH_SHOW_TAG=true
export SPACESHIP_TIME_SHOW=true
export SPACESHIP_EXIT_CODE_SHOW=true
export NIX_IGNORE_SYMLINK_STORE=1

#enhancd
export ENHANCD_DISABLE_HYPHEN=1

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

