#!/bin/bash

# $1 is the command to run if the application is not running yet
CMD="$1"
# $2 is the window class of the application
CLASS="$2"
# If a window of $CLASS already exists, toggle between minimized and focused;
# if no such window exists, call $CMD.

WINDOWS=($(xdotool search --class "$CLASS"))

if [ "${#WINDOWS[@]}" -eq 0 ]; then
    $CMD
    exit 0
fi

# if [ "${#WINDOWS[@]}" -gt 1 ]; then
#     kdialog --sorry "Multiple windows with class \"$CLASS\""
#     exit 1
# fi

if [[ " ${WINDOWS[@]} " =~ " $(xdotool getwindowfocus) " ]]; then
# if [ "${WINDOWS[0]}" -eq "$(xdotool getwindowfocus)" ]; then
  xdotool search --class "$CLASS" windowminimize %@
else
  xdotool search --class "$CLASS" windowactivate %@
fi
