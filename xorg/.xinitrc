[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
#eval "$(dbus-launch --sh-syntax --exit-with-session)"
export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
#nm-applet &
dropbox &
xmodmap ~/.Xmodmap &
ibus-daemon --xim &
pasystray &
clipit &
xset -b
xset r rate 200 40
feh --bg-scale ~/Dropbox/Screen/girls_with_guns_anime_girl_butterfly_101109_1920x1080.jpg &
#xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1920x1080 --dpi 500 --pos 0x0 --scale 1.5x1.5
exec xmonad
