if [[ "$OSTYPE" == "linux-gnu" ]]; then
    # ...
else [[ "$OSTYPE" == "darwin"* ]];
    # Mac OSX
    export ANDROID_HOME='/usr/local/share/android-sdk'
    export JAVA_HOME="$(/usr/libexec/java_home)"
    export GOPATH=$HOME/go
    export SCALA_HOME=/usr/local/opt/scala/idea
    export JDK_HOME="$(/usr/libexec/java_home)"
    export PATH=/usr/local/bin:$PATH
    export PATH=/usr/local/sbin:$PATH
    export PATH=~/.local/bin/:$PATH
    export PATH=~/git/leadiq-tools/aliases/:$PATH
    export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
fi

export GTAGSLABEL=pygments

# ssh
export SSH_KEY_PATH="~/.ssh/rsa_id"
export LC_ALL=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export TERM=xterm-256color

export GHTOKEN="ad38f8a815e974c98db2abd6f5ff304eca53400f"

# export https_proxy=http://127.0.0.1:64396
# export http_proxy=$https_proxy
# ftp_proxy = $https_proxy

#android
alias eml='/usr/local/share/android-sdk/emulator/emulator -avd Nexus_S_API_25'

# Postgress Env
export PGHOST=localhost
export PGPORT=5432
export PGDATA='/usr/local/var/postgres'

# Leadiq env
export DATABASE_URL="jdbc:postgresql://localhost/pg_dev"
export LEAD_SEARCH_ENABLE=true
export LEAD_SEARCH_HOST="localhost"
export LEAD_SEARCH_PORT="9300"
export LEAD_SEARCH_CLUSTER_NAME="elasticsearch_xiongchenyu"
export DATA_SCRIPTS_ES_CLUSTER_NAME="elasticsearch_xiongchenyu"
export DATA_SCRIPTS_ES_URI="localhost:9300"
#export LEADIQ_DEV_DOCKER_VOLUMES="/Volumes/mongo/docker"

# UniwebPay Env
export Uniwebpay_Mysql_U="root"
export Uniwebpay_Mysql_P=""
export Uniwebpay_Url="http://localhost:9000"

# amazon
alias vim='nvim'
alias cat=ccat
alias git=hub
alias yolo='git commit -m "$(curl -s whatthecommit.com/index.txt)"'
alias ef='f -e emacsclient -n'
alias ensime="gtags & sbt clean ensimeConfig test:compile ensimeServerIndex"

export SBT_OPTS="-Xmx8G"

export PKG_CONFIG_PATH="/usr/local/opt/libxml2/lib/pkgconfig"
