[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
export LANG=zh_CN.UTF-8
export LC_CTYPE=zh_CN.UTF-8
eval "$(dbus-launch --sh-syntax --exit-with-session)"
GTK_IM_MODULE=xim
QT_IM_MODULE=xim
export XMODIFIERS=@im=fcitx
stalonetray &
nm-applet &
dropbox &
xmodmap ~/.Xmodmap &
fcitx &
pasystray &
xset r rate 200 40
exec xmonad
