if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  exec ssh-agent startx
fi
eval "$(/opt/homebrew/bin/brew shellenv)"
