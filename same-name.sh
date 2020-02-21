#!/bin/bash
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
Usage: $CMDNAME [OPTION]... [--] [PATH]... NAME
Find all the files in PATHs (default: .) that match NAME.
Paths and file names with spaces are handled, but ';' is not.

-h, --help                  display this message.
-f, --find OPTS ;           pass OPTS to the find command (repeatable; default: '-type f').
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
            -f?*|--find=*)
                OPTS+=("${1#@(-f|--find=)}")
                ;& # fall through to the following case
            -f|--find)
                shift
                while [ $# -gt 0 ] ; do
                    if [ "$1" == ";" ] ; then
                        shift
                        break
                    fi
                    OPTS+=("$1")
                    shift
                done
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
    TPATH=()
    while [ $# -gt 1 ] ; do
        TPATH+=("$1")
        shift
    done
    if [ $# -eq 1 ] ; then
        NAME="$1"
        shift
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
    [ ${#OPTS[@]} -ne 0 ] || OPTS=(-type f)
    : ${TPATH[0]:=.}
}

main()
{
    find "${TPATH[@]}" "${OPTS[@]}" -name "$NAME" | sed 's|\(.*/\)\(.*\)|\1;\2|' | sort -t';' -k2 | awk -F ';' 'name == $2 { print toprint $1 $2; toprint = ""; next; } { name = $2; toprint = $1 $2 "\n"; }'
}

parse_cmd "$@"
main
