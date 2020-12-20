
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="zh_CN.utf8"
export LC_COLLATE="en_US.UTF-8"

export INPUT_METHOD=fcitx5
export GTK_IM_MODULE=fcitx5
export GTK3_IM_MODULE=fcitx5
export QT_IM_MODULE=fcitx5
export XMODIFIERS=@im=fcitx5
export XIM=fcitx5

#clipit &
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
