# where is antibody keeping its stuff?
ANTIBODY_HOME="$(antibody home)"

# set the theme to something, or blank if you use a non omz theme
#ZSH_THEME=

# you can do plugins the omz way if you want... or load with antibody later
#plugins=(archlinux aws brew cabal catimg colored-man-pages colorize command-not-found copyfile docker docker-compose direnv extract encode64 emacs fzf fasd fancy-ctrl-z git git-flow git-auto-fetch git-hubflow github gitignore gpg-agent golang httpie heroku jsontools kubectl npm node pass pipenv pip ripgrep redis-cli sbt scala stack systemd tmux)

# tell omz where it lives
#export ZSH="$ANTIBODY_HOME"/https-COLON--SLASH--SLASH-github.com-SLASH-xiongchenyu6-SLASH-oh-my-zsh

# quit bugging me!
#DISABLE_AUTO_UPDATE="true"

# antibody bundle < .zsh_plugins.txt > .zsh_plugins.sh
source ~/.zsh_plugins.sh

#eval "$(starship init zsh)"

# eval "$(jenv init -)"

# If not running interactively, do not do anything
#if [[ -z "$TMUX" ]] ;then
#    ID="$( tmux ls | grep -vm1 attached | cut -d: -f1 )" # get the id of a deattached session
#    if [[ -z "$ID" ]] ;then # if not available create a new one
#        tmux new-session
#    else
#        tmux attach-session -t "$ID" # if available attach to it
#    fi
#fi
#
#if [[ "$OSTYPE" == "darwin"* ]]; then
#    #alias grep='ggrep'
#fi

#alias vi='nvim'
#alias yolo='git commit -m "$(curl -s whatthecommit.com/index.txt)"'
#alias op='xdg-open'
#alias ls="exa --icons"
# alias tnpm="npm --registry https://mirrors.tencent.com/npm/"
# alias python=/usr/local/bin/python3
# alias cat=ccat
#
#
