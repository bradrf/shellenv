# Environment
# ###########

export CLICOLOR=1
export EDITOR=emacs
export GEM_HOME=/opt/gemrepo
export AWS_CONFIG_FILE="${HOME}/creds/aws-brad.conf"

# If this shell is interactive, turn on programmable completion enhancements.  Any completions you
# add in ~/.bash_completion are sourced last.
case $- in
    *i*)
	[[ -f /etc/bash_completion ]] && . /etc/bash_completion
	[[ -f /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion
	[[ -f /usr/local/bin/aws_completer ]] && complete -C aws_completer aws
	;;
esac

# Sets up the Bash prompt to better display the current working directory as well as exit status
# codes from failed commands.
export PROMPT_COMMAND="
  LASTEXIT=\$?;
  printf \"\e[32m\${USER}@\${HOSTNAME}\";
  [ \$LASTEXIT -ne 0 ] && printf \" \e[1;31m[\${LASTEXIT}]\e[0m\";
  printf \" \e[33m\${PWD}\e[0m\n\";"
export PS1='> '
export PS2=' '

# Prefer these directories to be at the top of the PATH.
for d in \
    '/usr/local/bin' \
    './node_modules/.bin' \
    './bin'
do
    export PATH="${d}:$(echo "$PATH" | sed -E "s#(^|:)${d}:#\1#")"
done

# Add directories to PATH if they exist.
for d in \
    "${HOME}/bin" \
    '/opt/gemrepo/bin' \
    '/usr/local/share/npm/bin'
do
    if [ -d "$d" ]; then
        echo "$PATH" | grep -qE ":${d}(:|\$)" || export PATH="${PATH}:$d"
    fi
done

UNAME=`uname`
if [ "$UNAME" = 'Darwin' ]; then
    DARWIN=true
else
    DARWIN=false
fi


# Shell Options
# #############

# notify immediately of background job state changes
set -b
shopt -s cdspell
shopt -s checkwinsize
# this is necessary for called things like ruby to access the var...
export COLUMNS
export LINES


# History Options
# ###############

HISTCONTROL=ignoredups:ignorespace:erasedups
HISTSIZE=10000
HISTFILESIZE=10000
shopt -s histappend


# Aliases
# #######

alias l='ls -hal'
alias ll='ls -al'
alias ps='myps'
alias ssh='retitlessh'
alias less='less -Rginm'
alias funcs='declare -F | grep -vF "declare -f _"'
alias func='declare -f'
alias ppath="echo \"\$PATH\" | tr ':' '\n'"
alias count_files='find -name .symform -prune -o -type f -print | wc -l'
alias rcopy='rsync -avzC'
alias reload='exec bash -l'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias homeshick="${HOME}/.homesick/repos/homeshick/home/.homeshick"
# let homeshick occasionally notify when it needs to be updated
homeshick --quiet refresh

if $DARWIN; then
    alias find='osxfind'
    alias netstat='osxnetstat'
fi


# Functions
# #########

myps()
{
    \ps auxwwww | awk '{if(NR==1 || (tolower($0) ~ /'"$*"'/ && ! / awk .if.NR/)){print}}'
}

# Automatically add in current directory if none was provided (act like GNU find).
osxfind()
{
    local arg
    if [ $# -gt 0 -a ! -d "$1" ]; then
	\find . "$@"
    else
	\find "$@"
    fi
}

# OS X's netstat isn't as useful as Linux's. This reports listeners correctly.
osxnetstat()
{
    local OPTIND OPTARG OPTERR opt sudo
    local args=(-i)

    while getopts 'pantu' opt; do
        case $opt in
            p) ;;
            a) sudo=sudo;;
            n) args=("${args[@]}" -nP);;
            t) args=${args[@]/-i/-iTCP};;
            u) args=${args[@]/-i/-iUDP};;
            ?)
                echo "usage: osxnetstat [pantu]" 1>&2
                return 1
        esac
    done

    $sudo lsof ${args[@]}
}

# Changes the terminal's and screen's titles to whatever text passed in (or to the previously set
# title if no arguments are provided).
RETITLE_DEFAULT=sh
RETITLE_PREVIOUS=sh
retitle()
{
    local title
    if [ $# -lt 1 ]; then
        title="$RETITLE_DEFAULT"
    else
        RETITLE_DEFAULT="$RETITLE_PREVIOUS"
        title="$*"
    fi
    RETITLE_PREVIOUS="$title"
    # conditionally sets both a terminal title and a screen title
    [ "$TERM" = 'screen' ] && echo -n -e "\\033k${title}\\033\\"
    printf "\\033]0;${title}\\007"
}

# call retitle with ssh info and reset back to original on exit
retitlessh()
{
    local name rc
    # try picking out the short hostname of the last argument
    name="${@: -1}"
    name="${name#*@}"
    name="${name%%.*}"
    [ -n "$name" ] && retitle "$name"
    \ssh "$@"
    rc=$?
    [ -n "$name" ] && retitle
    return $rc
}

# method to use without needing curl or wget
rawhttpget()
{
    local path

    if [ $# -ne 1 -a $# -ne 2 ]; then
        echo "usage: httpget <host> [<path>]" 1>&2
        return 1
    fi

    if [ $# -eq 2 ]; then
        path='index.html'
    else
        path="$2"
    fi

    printf "GET /${path} HTTP/1.1\r\nHost: ${1}\r\n\r\n" | nc $1 80
}

# figure out which download tool to use
if which curl >/dev/null 2>&1; then
    httpget='curl -k'
else
    httpget='wget -q --no-check-certificate -O -'
fi

getmyip()
{
    local scheme
    if [ $# -eq 0 ]; then scheme=http; else scheme=https; fi
    echo "$httpget \"${scheme}://ip.appspot.com\""
    $httpget "${scheme}://ip.appspot.com"
}

function hgdiff()
{
    local args
    if [ "$1" == '--' ]; then
        shift
    else
        args='-w'
    fi
    hg diff $args "$@" | colordiff | less
}

# Recursively grep files found but skipping the .svn directories. Can limit the scope of files to
# look at by providing additional find arguments (e.g. -name '*.cs' to look in only C# files).
function search()
{
    local find_args grep_args basedir cmd ext extprefix

    if [ "$1" = '-d' ]; then
        shift; basedir=$1; shift
    else
        basedir=.
    fi

    find_args=
    if [ "$1" = '-f' ]; then
        shift; find_args="${find_args} $1"; shift
    fi

    if [ "$1" = '-e' ]; then
        shift
        find_args="${find_args} \("
        for ext in `echo "$1" | sed 's/,/ /g'`; do
            find_args="${find_args} ${extprefix}-name '*.${ext}'"
            extprefix='-o '
        done
        find_args="${find_args} \)"
        shift
    fi

    if [ $# -lt 1 ]; then
        echo "usage: search [-d <basedir>] [-f <find_exp>] [-e <ext>[,<ext>]] <grep_args>" 1>&2
        return 1
    fi

    cmd="find '${basedir}' \( -name .svn -o -name .git \) -prune -o -type f${find_args} -print0 | \
xargs -0 grep -n --color=auto $*"
    echo "$cmd"
    eval $cmd
}

function etagsgen()
{
    local arg msg
    rm -f TAGS CSTAGS
    echo 'generating TAGS...'
    find . \( -name '*.h' -o -name '*.c' -o -name '*.cc' \) -print0 | xargs -0 etags -a
    if [ -f TAGS ]; then
        arg='-f CSTAGS'
    else
        msg=' (as TAGS) '
    fi
    echo "generating CSTAGS${msg}..."
    find . -name '*.cs' -print0 | xargs -0 etags -a $arg
}
