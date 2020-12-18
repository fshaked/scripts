#!/usr/bin/env bash

usage()
{
    cat <<ENDUSAGE
Usage: $CMDNAME CLASS CMD [ARG]...
Focus or minimize windows of CLASS. If no such window exists, use 'CMD [ARG]...' to create one.

-h, --help                  display this message.
ENDUSAGE
}

inodeof()
{
    [ -z "$1" ] || stat -c '%i' "$1"
}

parse_cmd()
{
    CMDNAME="${0##*/}"
    if [ "$(inodeof "$(command -v "$CMDNAME")")" != "$(inodeof "$0")" ] ; then
        CMDNAME="$0"
    fi

    args=$(getopt -o 'h' -l 'help' --name "${CMDNAME}" -- "$@")
    if [ $? -ne 0 ]; then
        usage >&2
        exit 2
    fi
    # Note the quotes around "$args": they are essential!
    eval set -- "$args"
    unset args

    while [ $# -gt 0 ] ; do
        case "$1" in
            '-h'|'--help')
                shift
                usage
                exit 0
                ;;
            '--')
                shift
                break
                ;;
            *)
                echo "${CMDNAME}: internal error!" >&2
                exit 1
                ;;
        esac
    done

    if [ $# -ge 2 ] ; then
        readonly CLASS="$1"
        shift
        readonly CMD=("$@")
    else
        usage >&2
        exit 2
    fi

    # Default values:
    : ${OPT:="???"}
}

main()
{
    WINDOWS=($(xdotool search --class "$CLASS"))

    if [ "${#WINDOWS[@]}" -eq 0 ]; then
        "${CMD[@]}"
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

parse_cmd "$@"
main
