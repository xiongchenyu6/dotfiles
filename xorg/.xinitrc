[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

export INPUT_METHOD=fcitx
export GTK_IM_MODULE=fcitx
export GTK3_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
export XIM=fcitx

clipit &
nm-applet &
dropbox &
xmodmap ~/.Xmodmap &
fcitx5 -d &
pasystray &
/usr/lib/gsd-xsettings &
xset -b
xset r rate 180 60
#xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1920x1080 --dpi 500 --pos 0x0 --scale 1.5x1.5
exec xmonad
