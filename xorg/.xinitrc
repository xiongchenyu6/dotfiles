[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
export LANG=zh_CN.UTF-8
export LC_CTYPE=zh_CN.UTF-8
eval "$(dbus-launch --sh-syntax --exit-with-session)"
GTK_IM_MODULE=xim
QT_IM_MODULE=xim
export XMODIFIERS=@im=fcitx
nm-applet &
dropbox &
xmodmap ~/.Xmodmap &
fcitx &
pasystray &
xset -b
xset r rate 200 40
xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1920x1080 --dpi 500 --pos 0x0 --scale 1.5x1.5
exec xmonad
