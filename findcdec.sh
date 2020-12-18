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
Usage: $CMDNAME [OPTION]... [--] PATERNS FILE[...]
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
#             -o|--opt)
#                 if [ $# -gt 1 ] ; then
#                     OPT="$2"
#                     shift 2
#                 else
#                     missingarg "$1"
#                     shift
#                 fi
#                 ;;
#             -o*|--opt=*)
#                 # requires 'shopt -s extglob'
#                 OPT="${1#@(-o|--opt=)}"
#                 shift
#                 ;;
            *)
                break
                ;;
        esac
    done
    OPTIONS=("$@")
}

parse_args()
{
    if [ $# -eq 1 ] ; then
        PATERNS="$1"
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
#     : ${OPT:="???"}
}

main()
{
    find . -name '*.c' -or -name '*.cc' -or -name '*.h' -or -name '*.hh'| xargs clang-check -ast-dump -ast-dump-filter "$PATERNS" 2> /dev/null | grep -e Decl | grep --color -e " ${PATERNS}\( \|$\)"
    # find . -name '*.c' -or -name '*.h' | xargs clang-check -ast-dump -ast-list 2> /dev/null | sort -u
    find . -name '*.c' -or -name '*.cc' -or -name '*.h' -or -name '*.hh' | xargs grep -i "#define[[:space:]]\+${PATERNS}\( \|$\)"
}

parse_cmd "$@"
main
