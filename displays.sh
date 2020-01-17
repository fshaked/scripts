#!/bin/bash

###  Laptop screen below external monitor
# Laptop output is eDP-1, native resolution is 3840x2160
# Monitor output is HDMI-1, native resolution is 2560x1440
# KDE Plasma System Settings -> Displays is configured to use the native resolutions 
# and Scale Display (both) by factor of 2.
# After plugging the external monitor, the lines below will configure the monitor 
# to use a framebuffer double the size of the native resolution and scale it down 
# to the native resolution, effectively canceling Plasma's scaling for the monitor.
xrandr --output HDMI-1 --scale 2x2 --mode 2560x1440 --fb 5120x2880 --pos 0x0
xrandr --screen 0 --fb 5120x5040
# where the height 5040 is 2880+2160
xrandr --output eDP-1 --pos 0x2880
