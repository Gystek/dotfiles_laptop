setxkbmap -option caps:escape
xmodmap -e "keysym Shift_R = Multi_key"

while xsetroot -name "$(/home/gustek/sources/dotfiles/dwm/modeline.sh)"
do
    sleep 5
done &

exec dbus-launch dwm 
