#!/bin/bash

# $1 is the command to run if the application is not running yet
CMD="$1"
# $2 is the window class of the application
CLASS="$2"
# If a window of $CLASS already exists, toggle between minimized and focused;
# if no such window exists, call $CMD.

WINDOW="$(xdotool search --limit 1 --class "$CLASS")"
FOUND="$?"
if [ "$FOUND" -ne 0 ]; then
    $CMD
    exit 0
fi

if [ "$WINDOW" -eq "$(xdotool getwindowfocus)" ]; then
  xdotool windowminimize "$WINDOW"
else
  xdotool windowactivate "$WINDOW"
fi
