#!/usr/bin/env bash

# set -x

usage()
{
    cat <<ENDUSAGE
Usage: $CMDNAME [OPTION]... [--] ARG1 ARG2
Descritopn...

-h, --help                  display this message.
-o, --opt OPT               (default: ???) description...
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

    if ! args=$(getopt -o 'ho:' -l 'help,opt:' --name "${CMDNAME}" -- "$@"); then
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
            '-o'|'--opt')
                readonly OPT="$2"
                shift 2
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

    if [ $# -eq 2 ] ; then
        readonly ARG1="$1"
        readonly ARG2="$2"
        shift 2
    else
        usage >&2
        exit 2
    fi

    # Default values:
    : ${OPT:="???"}
}

main()
{
    echo TODO
}

parse_cmd "$@"
main
