#!/bin/bash
# extglob is needed for "@(-o|--opt=)"
shopt -s extglob

inodeof()
{
    [ -z "$1" ] || stat -c '%i' "$1"
}

CMDNAME="${0##*/}"
if [ "$(inodeof "$(which "$CMDNAME")")" != "$(inodeof "$0")" ] ; then
    CMDNAME="$0"
fi

usage()
{
    cat <<ENDUSAGE
Usage: $CMDNAME CMD CLASS
Focus or minimize windows of CLASS. If no such window exists, use CMD to create one.

-h, --help                  display this message.
ENDUSAGE
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
    if [ $# -eq 2 ] ; then
        CMD="$1"
        CLASS="$2"
        shift 2
    else
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
}

parse_args "$@"
main
