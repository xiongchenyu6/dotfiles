# see base omz config: https://github.com/robbyrussell/oh-my-zsh/blob/master/templates/zshrc.zsh-template

# where is antibody keeping its stuff?
ANTIBODY_HOME="$(antibody home)"

# set the theme to something, or blank if you use a non omz theme
ZSH_THEME=

# you can do plugins the omz way if you want... or load with antibody later
plugins=(fzf fasd brew emacs tmux git git-flow git-hubflow github gitignore scala stack sbt aws docker docker-compose colored-man-pages colorize extract colorize archlinux)

# tell omz where it lives
export ZSH="$ANTIBODY_HOME"/https-COLON--SLASH--SLASH-github.com-SLASH-xiongchenyu6-SLASH-oh-my-zsh

# quit bugging me!
DISABLE_AUTO_UPDATE="true"

source ~/.zsh_plugins.sh

eval "$(fasd --init posix-alias zsh-hook)"
# If not running interactively, do not do anything
if [[ -z "$TMUX" ]] ;then
    ID="$( tmux ls | grep -vm1 attached | cut -d: -f1 )" # get the id of a deattached session
    if [[ -z "$ID" ]] ;then # if not available create a new one
        tmux new-session
    else
        tmux attach-session -t "$ID" # if available attach to it
    fi
fi
