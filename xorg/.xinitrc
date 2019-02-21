[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
#eval "$(dbus-launch --sh-syntax --exit-with-session)"
export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export LC_CTYPE="zh_CN.utf8"
export LC_COLLATE="en_US.UTF-8"

export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export XIM=ibus
export QT_IM_MODULE=ibus
export XIM_ARGS="ibus-daemon -d -x"

#nm-applet &
dropbox &
xmodmap ~/.Xmodmap &
ibus-daemon -d -x &
pasystray &
clipit &
xset -b
xset r rate 200 40
feh --bg-scale ~/Dropbox/Screen/girls_with_guns_anime_girl_butterfly_101109_1920x1080.jpg &
#xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1920x1080 --dpi 500 --pos 0x0 --scale 1.5x1.5
exec xmonad
