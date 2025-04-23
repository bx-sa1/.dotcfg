#!/bin/bash

# xfce4-genmon script to monitor current memory usage
# 2020 (ɔ) almaceleste

# mem usage threshold warning (in GB) - yellow
warn=70
# mem usage threshold alarm (in GB) - red
alarm=90

percent=$(free | grep Mem | awk '{printf "%d", ($3/$2)*100}')
used=$(free --giga | grep Mem | awk '{printf "%d", $3}')
free=$(free --human --giga | grep Mem | awk '{printf "%s", $4}')
shared=$(free --human --giga | grep Mem | awk '{printf "%s", $5}')
avail=$(free --human --giga | grep Mem | awk '{printf "%s", $7}')

color='lightgrey'
if [ $percent -gt $alarm ]
then
    color='red'
elif [ $percent -gt $warn ]
then
    color='yellow'
fi
percent="${percent}%"

echo "<txt><span foreground="\'$color\'"> $percent </span></txt><txtclick>xfce4-taskmanager</txtclick>"
echo -e "<tool>mem: \t${used}G used\n\t\t$free free\n\t\t$shared shared\n\t\t$avail avail</tool>"
