[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
#eval "$(dbus-launch --sh-syntax --exit-with-session)"
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
#nm-applet &
dropbox &
xmodmap ~/.Xmodmap &
ibus-daemon --xim &
#pasystray &
xset -b
xset r rate 200 40
#xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1920x1080 --dpi 500 --pos 0x0 --scale 1.5x1.5
exec xmonad
