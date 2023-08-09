#!/bin/sh

mod_battery() {
    local sign=""

    if [ ! -z "$(acpi -b | egrep 'Discharging')" ]
    then
        sign="-"
    elif [ ! -z "$(acpi -b | egrep 'Charging')" ]
    then
        sign="+"
    fi

    local level=$(acpi -b | egrep -o "[0-9]?[0-9]?[0-9]%")

    printf "%s%s" "$sign" "$level"
}

mod_date() {
    date +"%a %d %b, %Y    %H:%M"
}

mod_mem() {
    free -h | head -n 2 | tail -n 1 | awk -F' ' '{ print $7"/"$2 }'
}

echo "  $(mod_mem) | $(mod_battery) | $(mod_date)  "
