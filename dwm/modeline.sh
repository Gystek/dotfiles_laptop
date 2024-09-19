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

mod_day_name() {
    date +"%a"
}

mod_time() {
    date +"%H:%M"
}

mod_gregorian_date() {
    date +"%d %b %Y"
}

mod_hijri_date() {
    hijridate
}

mod_ash() {
    ash
}

mod_mem() {
    free -h | head -n 2 | tail -n 1 | awk -F' ' '{ print $7"/"$2 }'
}

echo "  $(mod_mem) | $(mod_battery) | $(mod_ash) | $(mod_day_name) $(mod_hijri_date) ($(mod_gregorian_date)) $(mod_time)  "
