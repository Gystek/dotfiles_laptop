#!/bin/sh

process_file() {
    local source="$1"
    local dist="$2"

    if [ -f "$source" ]
    then
        ln -s "$(pwd)/$source" "$2"
    elif [ -d "$source" ]
    then
        if [ -d "$dist" ]
        then
            cd $(dirname "$source")
            stow $(basename "$source") -t "$dist"
            cd -
        else
            printf "Cannot stow a directory to a file.\n" > /dev/stderr
            exit 1
        fi
    else
        printf "Cannot find filesystem element \`%s'.\n" "$source" > /dev/stderr
    fi
}

process_file xorg "$HOME"
process_file git  "$HOME"
process_file fish "$HOME/.config/fish"
