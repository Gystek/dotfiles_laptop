setxkbmap us -variant workman
xmodmap -e "keysym Alt_R = Multi_key"

feh --bg-max ~/documents/pictures/Могучая_даль.jpg

while xsetroot -name "$(/home/gustek/sources/dotfiles/dwm/modeline.sh)"
do
    sleep 5
done &

exec dbus-launch dwm 
