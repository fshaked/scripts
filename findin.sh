#!/bin/bash

# extglob is needed for "@(-o|--opt=)"
shopt -s extglob

find "${myopt[@]}"
exit 0

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
Usage: $CMDNAME -n NAME [OPTION]... [--] PATTERNS
Grep for PATTERNS in all files matching any of NAMEs.

-h, --help                  display this message.
-n, --name NAME             file name pattern (repeatable)
-f, --find OPTS ;           pass OPTS to the find command (repeatable).
-g, --grep OPTS ;           pass OPTS to the grep command (repeatable).
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
    FINDNAMES=(-false)
    while [ $# -gt 0 ] ; do
        case "$1" in
            -h|--help)
                shift
                usage
                exit 0
                ;;
            -n|--name)
                if [ $# -gt 1 ] ; then
                    FINDNAMES+=(-o -name "$2")
                    shift 2
                else
                    missingarg "$1"
                fi
                ;;
            -n*|--name=*)
                # requires 'shopt -s extglob'
                FINDNAMES+=(-o -name "${1#@(-o|--opt=)}")
                shift
                ;;
            -f?*|--find=*)
                FOPTS+=("${1#@(-f|--find=)}")
                ;& # fall through to the following case
            -f|--find)
                shift
                while [ $# -gt 0 ] ; do
                    if [ "$1" == ";" ] ; then
                        shift
                        break
                    fi
                    FOPTS+=("$1")
                    shift
                done
                ;;
            -f?*|--find=*)
                GOPTS+=("${1#@(-g|--grep=)}")
                ;& # fall through to the following case
            -g|--grep)
                shift
                while [ $# -gt 0 ] ; do
                    if [ "$1" == ";" ] ; then
                        shift
                        break
                    fi
                    GOPTS+=("$1")
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
    PATTERNS=()
    while [ $# -gt 0 ] ; do
        PATTERNS+=(-e "$1")
        shift
    done
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

    if [ "${#FINDNAMES[@]}" -eq 1 ] || [ "${#PATTERNS[@]}" -eq 0 ] ; then
        usage
        exit 2
    fi
}

main()
{
    find "${FOPTS[@]}" \( "${FINDNAMES[@]}" \) -exec grep --color --with-filename --line-number --extended-regexp "${GOPTS[@]}" "${PATTERNS[@]}" '{}' \;
}

parse_cmd "$@"
main
