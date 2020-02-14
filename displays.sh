#!/bin/bash


# extglob is needed for "@(-o|--opt=)"
shopt -s extglob

CMDNAME="${0##*/}"
if [ "$(which "$CMDNAME")" != "$(realpath "$0")" ] ; then
    CMDNAME="$0"
fi

usage()
{
    cat <<ENDUSAGE
Usage: $CMDNAME
Change the displays configuration based on the current host-name

-h, --help                  display this message.
ENDUSAGE
}

missingarg()
{
    echo "${CMDNAME}: option requires an argument -- '$1'"
    usage
    exit 2
}

parse_options()
{
    while [ $# -gt 0 ] ; do
        case "$1" in
            -h|--help)
                shift
                usage
                exit 0
                ;;
            *)
                break
                ;;
        esac
    done
    OPTIONS=("$@")
}

parse_args()
{
    if [ $# -ne 0 ] ; then
        usage
        exit 2
    fi
}

parse_cmd()
{
    OPTIONS=()
    while [ $# -gt 0 ] ; do
        if [ "$1" == "--" ] ; then
            shift
            break
        fi
        OPTIONS+=("$1")
        shift
    done
    parse_options "${OPTIONS[@]}"
    parse_args "${OPTIONS[@]}" "$@"
}

main()
{
    HOSTNAME="$(hostname)"
    SCREEN="$(currentscreen)"

    # Associative array, maps connected ports to resolutions
    # Example: PORTS["DP-1"]="2560 1440"
    declare -A PORTS
    while read -a fields; do
        PORTS["${fields[0]}"]="${fields[1]} ${fields[2]}"
    # done < <(xrandr --current | sed -nE ': begin /^[^ ]+ connected/ { s/(^[^ ]+) .*/\1/ ; h ; n ; /^   [0-9]+x[0-9]+ .*/ { s/^   ([0-9]+)x([0-9]+) .*/\1 \2/ ; H ; x ; y/\n/ / ; p ; b } ; b begin }')
    done < <(xrandr --current | awk '!/^ / { port = ($2 == "connected" ? $1 : "") } port != "" && $1 ~ /[[:digit:]]+x[[:digit:]]+/ { res=$1; sub(/x/," ",res); print port, res; port = "" }')

    case "$HOSTNAME" in
        sflur) sflur ;;
    esac

    NEWSCREEN="$(currentscreen)"
    if [ "$SCREEN" != "$NEWSCREEN" ]; then
        notify-send -a displays.sh -t 10000 "Displays changed" "Displays configuration changed to ${NEWSCREEN}.\n$(xrandr --current | sed -n '/ connected [1-9]/ { s/+.*//; s/ connected / /p }')"
    fi
}

# Get the current screen resolution
# Example: "3840 x 2160"
currentscreen() {
    xrandr --current | sed  -n 's/^Screen 0:.*current \([0-9]\+ x [0-9]\+\).*/\1/p'
}

twoscreens() {
    pport="$1"
    pwidth="$2"
    pheight="$3"
    pscale="$4"

    sport="$5"
    swidth="$6"
    sheight="$7"
    sscale="$8"

    fbwidth="$((pwidth*pscale > swidth*sscale ? pwidth*pscale : swidth*sscale))"
    fbheight="$((pheight*pscale + sheight*sscale))"

    if [ "$SCREEN" = "${fbwidth} x ${fbheight}" ] ; then
        xrandr --screen 0 --fb "$((pwidth*pscale))x$((pheight*pcale))" --output "$pport" --mode "${pwidth}x${pheight}" --scale "${pscale}x${pscale}" --pos 0x0 --output "$sport" --off
    else
        xrandr --screen 0 --fb "${fbwidth}x${fbheight}" --output "$pport" --mode "${pwidth}x${pheight}" --scale "${pscale}x${pscale}" --pos "0x$((sheight*sscale))" --output "$sport" --mode "${swidth}x${sheight}" --scale "${sscale}x${sscale}" --pos 0x0
    fi
}

sflur()
{
###  Laptop screen below external monitor
# Laptop output is eDP-1, native resolution is 3840x2160
# Monitor output is DP-1, native resolution is 2560x1440
# KDE Plasma System Settings -> Displays is configured to use the native resolutions
# and Scale Display (both) by factor of 2.
# After plugging the external monitor, the lines below will configure the monitor
# to use a framebuffer double the size of the native resolution and scale it down
# to the native resolution, effectively canceling Plasma's scaling for the monitor.
#
#      ├────5120 (=2560x2)────┤
#    ┬ ┌──────────────────────┐ ┬
#    │ │Monitor (DP-1)        │ │
#    │ │                      │ │
#    │ │                      │ 2880 (=1440x2)
#    │ │                      │ │
#    │ │                      │ │
# 5040 ├───────────────┬──────┘ ┼
#    │ │Laptop (eDP-1) │        │
#    │ │               │        │
#    │ │               │        2160
#    │ │               │        │
#    ┴ └───────────────┘        ┴
#      ├─────3840──────┤

    if [ -n "${PORTS["DP-1"]}" ] ; then
        # External monitor is connected
        # 1920 1080
        twoscreens eDP-1 3840 2160 1 DP-1 ${PORTS["DP-1"]} 2
    elif [ -n "${PORTS["DP-2"]}" ] ; then
        # External monitor is connected
        twoscreens eDP-1 3840 2160 1 DP-2 ${PORTS["DP-2"]} 2
    elif [ -n "${PORTS["HDMI-1"]}" ] ; then
        # External monitor is connected
        twoscreens eDP-1 3840 2160 1 HDMI-1 ${PORTS["HDMI-1"]} 2
    else
        # No external monitor
        xrandr --screen 0 --fb 3840x2160 --output eDP-1 --mode 3840x2160 --scale 1x1 --pos 0x0
    fi
}

parse_cmd "$@"
main
