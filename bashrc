# Emacs, this is -*-shell-script-*-
# add the following lines to the end of ~/.bashrc
# if [ -f "$HOME/scripts/bashrc" ]; then
#     . "$HOME/scripts/bashrc"
# fi

shopt -s extglob

PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd ):$HOME/.local/bin:$HOME/bin:$PATH"

# ANSI color codes
RS="\[\033[0m\]"    # reset
HC="\[\033[1m\]"    # hicolor
UL="\[\033[4m\]"    # underline
INV="\[\033[7m\]"   # inverse background and foreground

FBLK="\[\033[30m\]" # foreground black
FRED="\[\033[0;31m\]" # foreground red
FGRN="\[\033[0;32m\]" # foreground green
FYEL="\[\033[0;33m\]" # foreground yellow
FBLE="\[\033[0;34m\]" # foreground blue
FMAG="\[\033[0;35m\]" # foreground magenta
FCYN="\[\033[0;36m\]" # foreground cyan
FWHT="\[\033[0;37m\]" # foreground white

FLBLK="\[\033[1;30m\]" # foreground black
FLRED="\[\033[1;31m\]" # foreground red
FLGRN="\[\033[1;32m\]" # foreground green
FLYEL="\[\033[1;33m\]" # foreground yellow
FLBLE="\[\033[1;34m\]" # foreground blue
FLMAG="\[\033[1;35m\]" # foreground magenta
FLCYN="\[\033[1;36m\]" # foreground cyan
FLWHT="\[\033[1;37m\]" # foreground white

BBLK="\[\033[0;40m\]" # background black
BRED="\[\033[0;41m\]" # background red
BGRN="\[\033[0;42m\]" # background green
BYEL="\[\033[0;43m\]" # background yellow
BBLE="\[\033[0;44m\]" # background blue
BMAG="\[\033[0;45m\]" # background magenta
BCYN="\[\033[0;46m\]" # background cyan
BWHT="\[\033[0;47m\]" # background white


export GIT_PS1_STATESEPARATOR=
export GIT_PS1_SHOWDIRTYSTATE=true
export GIT_PS1_SHOWSTASHSTATE=true

export PROMPT_COMMAND=__prompt_command  # Func to gen PS1 after CMDs

function __prompt_command() {
  local EXIT="$?"             # This needs to be first

  local git_ps="$([ -n "$(type -t __git_ps1)" ] && __git_ps1 " (%s)")"
  local line1="${FLYEL}\u@\h: ${FLGRN}\w${FLYEL}${git_ps}"
  local line2="${FLYEL}[\D{%H:%M}]"

  if [ $EXIT != 0 ]; then
    line2="${BRED}${line2} ${EXIT} ${RS}"
  fi

  if [ -n "$STY" ]; then
    # screen session
    line1="${BCYN}${line1}${RS}"
  elif [ -n "$SSH_CONNECTION" ]; then
    line1="${BMAG}${line1}${RS}"
  else
    line1="${BBLE}${line1}${RS}"
  fi

  PS1="${line1}\n${line2}${RS} "
}

bind 'set completion-ignore-case on'

bind    '"\eOM":"\eOHsudo \eOF\C-m"'       # shift-enter
bind -x '"\el":"l"'                        # alt-l

# Bash only checks the first word for alias expansion. The space at the
# end here tells Bash to also check the following word:
alias sudo='sudo '

# This is only meant to work as 'sudo aptup'
alias aptup="-s -- eval 'apt update && apt upgrade --auto-remove'"

alias ll='ls -alhF'
alias l='ls -lhF'
alias rm='rm -i'
alias vnc='x11vnc -localhost -usepw -forever'
alias ksvn='kdesvn ./'
alias beep='paplay ~/beep.wav'
alias ppd="date '+%Y-%m-%d'"

export HIGHLIGHT_BEEP='paplay ~/beep.wav'

if [ -n "$SSH_CONNECTION" ]; then
  export EDITOR='nano'
else
  export EDITOR='emacsclient'
fi

alias xo='xdg-open'
complete -o filenames -o plusdirs -fd -X '!*.pdf' xo

# used by rmem regression
export EMAIL='shaked.flur@cl.cam.ac.uk'

function sdu() { du --human-readable --max-depth 1 --all --total "$@" | sort -h; }

ff() { find -L . -name "$1"; }

fs() { find -L . -name "*$1"; }

findlemdef()
{
    local LET_PATTERN="^[[:space:]]*let[[:space:]]*($1)([[:space:]]|$)"
    local TYPE_PATTERN="^[[:space:]]*type[[:space:]]*($1)([[:space:]]|$)"
    findin.sh -g --after-context 10 \; -n "*.lem" -- "$LET_PATTERN" "$TYPE_PATTERN"
}

alias findc='findin.sh -n "*.c" -n "*.cc" -n "*.cpp" -n "*.h" -n "*.hh" -n "*.hpp" --'
alias findh='findin.sh -n "*.h" -n "*.hh" -n "*.hpp" --'
alias findlem='findin.sh -n "*.lem" --'
alias findml='findin.sh -n "*.ml" -n "*.mli" --'
alias findm='findin.sh -n "*.lem" -n "*.ml" -n "*.mli" --'
alias findsail='findin.sh -n "*.sail" --'
alias findtex='findin.sh -n "*.tex" --'

findhard()
{
    for FILE in "$@"; do
        INODE=$( ls -1 --inode "$FILE" | cut --delimiter ' ' --fields 1 | grep '[0-9][0-9]*' )
        echo $FILE \(inode $INODE\)
        find ~/ -xdev -inum $INODE
        echo
    done
}

export RMEMDIR="$HOME/rems/rmem/"
PATH="$RMEMDIR:$PATH"
_rmem()
{
  local cur prev opts
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"
  opts=$(rmem -help | sed -n 's/[[:space:]]*\(-[^[:space:]]*\).*/\1/p' | tr '\n' '\ ')
  boolopts=$(rmem -help | sed -n 's/[[:space:]]*\(-[^[:space:]]*\)[[:space:]]<bool>.*/\1/p' | tr '\n' '|')
  boolopts="${boolopts%?}"
  fileopts=$(rmem -help | sed -n 's/[[:space:]]*\(-[^[:space:]]*\)[[:space:]]<file>.*/\1/p' | tr '\n' '|')
  fileopts="${fileopts%?}"
  modeloptions=$(rmem -help | sed -n 's/^Model options\: \(.*\)$/\1/p' | tr -d ';')
  topoptions='[0] [0,1] [[0,1],2] [[0,2],1] [0,[1,2]] [[0,1],[2,3]] [[0,2],[1,3]] [[0,3],[1,2]] [[[0,1],2],3] [[[0,1],3],2] [[[0,2],1],3] [[[0,2],3],1] [[[0,3],1],2] [[[0,3],2],1] [[[1,2],0],3] [[[1,2],3],0] [[[1,3],0],2] [[[1,3],2],0] [[[2,3],0],1] [[[2,3],1],0]'

  case "${prev}" in
    @(${boolopts}))
      COMPREPLY=( $(compgen -W "true false" -- ${cur}) )
      return 0
      ;;
    @(${fileopts}))
      COMPREPLY=( $(compgen -f -o filenames -- ${cur}) )
      return 0
      ;;
    -model)
      COMPREPLY=( $(compgen -W "${modeloptions}" -- ${cur}) )
      return 0
      ;;
    -top)
      COMPREPLY=( $(compgen -W "${topoptions}" -- ${cur}) )
      return 0
      ;;
    *)
      ;;
  esac

  if [[ ${cur} == -* ]] ; then
    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
    return 0
  fi

  local IFS=$'\n'
  COMPREPLY=( $(compgen -o filenames -o plusdirs -f -X '!*.@(litmus|out)' -- ${cur}) $(compgen -o filenames -f -X '*.*' -- ${cur}) )
}
complete -o filenames -F _rmem rmem

alias debugrmem='ocamldebug $(find $RMEMDIR/_build -type d -printf "-I %p ") $(find $RMEMDIR/../lem/ocaml-lib/_build_zarith -type d -printf "-I %p ") -I $RMEMDIR/../linksem/src $(find $RMEMDIR/../linksem/src/abis -type d -printf "-I %p ") $RMEMDIR/main.d.byte'

complete -o filenames -F _rmem debugrmem

PATH="$HOME/rems/litmus-tests-regression-machinery/tools/bin/:$PATH"

_diyedges()
{
  shift
  es=($(diyone7 "$@" -show edges 2>&1))
  if [ "$?" != "0" ]; then
    echo error 1>&2
    COMPREPLY=()
    return
  fi

  COMPREPLY=( $(compgen -W "${es[*]}" -- "${COMP_WORDS[COMP_CWORD]}") )
}

_diyanns()
{
  shift
  local as=($(diyone7 "$@" -show annotations 2>&1))
  if [ "$?" != "0" ]; then
    echo error 1>&2
    COMPREPLY=()
    return
  fi

  local cc="${COMP_WORDS[COMP_CWORD]/"?"*/}"
  local anns=()
  for al in "${as[@]}"; do
    anns+=("${cc}?${al}P")
    anns+=("${cc}?P${al}")
    for ar in "${as[@]}"; do
      anns+=("${cc}?${al}${ar}")
    done
  done

  COMPREPLY=( $(compgen -W "${anns[*]}" -- "${COMP_WORDS[COMP_CWORD]}") )
}

_diyone7()
{
  cur="${COMP_WORDS[COMP_CWORD]}"
  case "${cur}" in
    -*)
      local opts=$(diyone7 -help | sed -n 's/[[:space:]]*\(-[^[:space:]]*\).*/\1/p' | tr '\n' '\ ')
      COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
      return 0
      ;;
    *"?"*)
      _diyanns "${COMP_WORDS[@]}"
      return 0
      ;;
  esac

  boolopts=$(diyone7 -help | sed -n 's/[[:space:]]*\(-[^[:space:]]*\)[[:space:]]<bool>.*/\1/p' | tr '\n' '|')
  boolopts="${boolopts%?}"

  case "${COMP_WORDS[COMP_CWORD-1]}" in
    @(${boolopts}))
      COMPREPLY=( $(compgen -W "true false" -- ${cur}) )
      return 0
      ;;
    -arch)
      local archs="$(diyone7 -help | sed -n '/^[[:space:]]*-arch[[:space:]]/ { s/^[[:space:]]*-arch[[:space:]]\+<\([^>]*\).*/\1/ ; s/|/ /pg }')"
      COMPREPLY=( $(compgen -W "${archs}" -- ${cur}) )
      return 0
      ;;
  esac

  _diyedges "${COMP_WORDS[@]}"
}
complete -F _diyone7 diyone7

diyo()
{
  local EXISTING="$(ls -1)"

  NAME="$(diyone7 -type uint64_t -variant ConstsInInit -norm -v -arch AArch64 $* 2>&1)"
  local EXIT=$?

  if [ "$EXIT" == 0 ]; then
    local NAME="$(echo "$NAME" | sed -n 's/^Test name: //p')"
    cat "${NAME}.litmus"

    echo "$EXISTING" | grep -qF "${NAME}.litmus"
    if [ "$?" == 0 ]; then
      echo
      echo "*** ${NAME}.litmus already exists ***"
    else
      { echo "# added by 'diyo'"
        echo "${NAME}.litmus"
      } >> @all
    fi
  else
    echo "$NAME" 1>&2
    return $EXIT
  fi
}
complete -F _diyone7 diyo

# open Jenkins
alias jenkins='xdg-open "https://svr-rems-build.cl.cam.ac.uk" && ssh -N -D 8080  slogin-serv.cl.cam.ac.uk'

new()
{
  PWD="$(pwd)"
  NEWSESS="$(qdbus org.kde.yakuake /yakuake/sessions addSession)"
  qdbus org.kde.yakuake /yakuake/sessions runCommandInTerminal "$NEWSESS" " cd '$PWD'" > /dev/null
}

alias sshpass='ssh -o PreferredAuthentications=password -o PubkeyAuthentication=no'

alias ggrep='git grep'

git-config-sflur()
{
    git config user.name "Shaked Flur"
    git config user.email sflur@google.com
}


ssh--()
{
    # Assume the last argument is the host name (or user@host).
    local host="${!#}"

    # Run gcert if needed
    if command -v gcert >/dev/null && ! gcertstatus -check_ssh=false ; then
        echo '!!! cert expaired, running gcert'
        gcert
    fi

    ssh -t -R 22042:localhost:22 "$@" -- "export MYSSHNAME=$host && bash -l"
}
complete -F _ssh ssh--

ec()
{
    if [[ -n "$SSH_TTY" ]]; then
        # Running over ssh, send back a command to open emacsclient using TRAMP
        ssh -p 22042 localhost -- emacsclient -n /ssh:${HOSTNAME}:$PWD/$1
    else
        emacsclient -n "$@"
    fi
}

alias enw='emacs -nw'

man()
{
    ec -e "(progn (man \"$1\") (select-frame-set-input-focus (selected-frame)))"
}

# apt get install speedtest-cli
alias st='speedtest --no-upload'

myhelp()
{
    echo "new         - open a new yakuake terminal in the same PWD."
    echo "xo FILE     - open FILE with the default application."
    echo "ff FILE     - search for FILE under the current dir and subdirs."
    echo "findh TEXT  - search for TEXT in all .h/hpp files under the current dir and subdirs."
    echo "findc TEXT  - search for TEXT in all .c/cpp files under the current dir and subdirs."
    echo "sshpass     - force ssh to authenticate with password."
    echo
    echo "Shift+Enter - add 'sudo' to the beginning and execute."
    echo "win+Enter   - add 'kdesudo' to the beginning and execute."
    echo "Ctrl+H      - add 'man' to the beginning of the command."
    echo "Ctrl+L      - clear the terminal."
    echo "Alt+L       - display files in current dir."
    echo
    echo "sudo ipsec up/down CAM - turn UNI VPN on/off"
    echo "sudo nethogs           - top-like network monitor"
}
# myhelp

################################################################################

# llvm tools from rust build
PATH="$HOME/workspace/rust-verification/rust/build/x86_64-unknown-linux-gnu/llvm/bin:$PATH"

# SeaHorn
PATH="$HOME/workspace/rust-verification/seahorn-llvm10/build/run/bin:$PATH"

# Crux (Galuas MIR verifier)
export CRUX_RUST_LIBRARY_PATH="$HOME/workspace/rust-verification/mir-verifier/rlibs"

# Gem (Ruby)
PATH="$HOME/.local/share/gem/ruby/2.7.0/bin:$PATH"

################################################################################

export PATH
