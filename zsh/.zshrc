if [[ "$OSTYPE" == "linux-gnu" ]]; then
    export ZSH=~/.oh-my-zsh
    ZSH_THEME="powerlevel9k/powerlevel9k"
    POWERLEVEL9K_MODE='nerdfont-complete'
    POWERLEVEL9K_VCS_SHOW_SUBMODULE_DIRTY=true
    POWERLEVEL9K_DIR_OMIT_FIRST_CHARACTER=true
    POWERLEVEL9K_DIR_WRITABLE_FORBIDDEN_FOREGROUND="white"
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=0
    POWERLEVEL9K_SHOW_CHANGESET=true
    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(root_indicator disk_usage dir dir_writable vcs)
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs command_execution_time)
    plugins=(fzf fasd brew emacs tmux git git-flow git-hubflow github gitignore scala stack sbt aws docker docker-compose colored-man-pages colorize extract zsh-autosuggestions zsh-syntax-highlighting zsh-wakatime colorize archlinux)
    eval "$(fasd --init posix-alias zsh-hook)"
    source $ZSH/oh-my-zsh.sh
    # If not running interactively, do not do anything
    if [[ -z "$TMUX" ]] ;then
        ID="$( tmux ls | grep -vm1 attached | cut -d: -f1 )" # get the id of a deattached session
        if [[ -z "$ID" ]] ;then # if not available create a new one
            tmux new-session
        else
            tmux attach-session -t "$ID" # if available attach to it
        fi
    fi
else [[ "$OSTYPE" == "darwin"* ]];
    # Mac OSX
    ZSH_THEME="powerlevel9k/powerlevel9k"
    POWERLEVEL9K_MODE='nerdfont-complete'
    POWERLEVEL9K_VCS_SHOW_SUBMODULE_DIRTY=true
    POWERLEVEL9K_DIR_OMIT_FIRST_CHARACTER=true
    POWERLEVEL9K_DIR_WRITABLE_FORBIDDEN_FOREGROUND="white"
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=0
    POWERLEVEL9K_SHOW_CHANGESET=true
    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(root_indicator disk_usage dir dir_writable vcs)
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status background_jobs command_execution_time)
    plugins=(zsh-wakatime osx fasd brew emacs tmux git git-flow git-hubflow github gitignore scala stack sbt aws docker docker-compose colored-man-pages colorize extract zsh-autosuggestions zsh-syntax-highlighting zsh-256color osx fzf)
    eval "$(fasd --init posix-alias zsh-hook)"
    export PKG_CONFIG_PATH=/usr/local/Cellar/pkg-config/0.29.2/bin/pkg-config:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig
    test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
    source $ZSH/oh-my-zsh.sh
    . ~/.nix-profile/etc/profile.d/nix.sh
fi
