[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
stalonetray &
fcitx &
xmodmap ~/.Xmodmap &
exec xmonad
~                                                          
