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
Usage: $CMDNAME [OPTION]... [--] ARG1 ARG2
Descritopn...

-h, --help                  display this message.
-o, --opt OPT               (default: ???) description...
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
            -o|--opt)
                if [ $# -gt 1 ] ; then
                    OPT="$2"
                    shift 2
                else
                    missingarg "$1"
                    shift
                fi
                ;;
            -o*|--opt=*)
                OPT+=("${1#@(-o|--opt=)}")
                shift
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
        ARG1="$1"
        ARG2="$2"
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

    # Default values:
    : ${OPT:="???"}
}

main()
{
}

parse_cmd "$@"
main
