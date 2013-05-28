# Environment
# ###########

export CLICOLOR=1
export EDITOR=emacs
export GEM_HOME=/opt/gemrepo

# If this shell is interactive, turn on programmable completion enhancements.
# Any completions you add in ~/.bash_completion are sourced last.
case $- in
    *i*)
	[[ -f /etc/bash_completion ]] && . /etc/bash_completion
	[[ -f /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion
	;;
esac

# Sets up the Bash prompt to better display the current working
# directory as well as exit status codes from failed commands.
export PROMPT_COMMAND="
  LASTEXIT=\$?;
  printf \"\e[32m\${USER}@\${HOSTNAME}\";
  [ \"\$AWS_ACCOUNT\" = 'production' ] && printf \" \e[1;36m[\${AWS_ACCOUNT}]\e[0m\";
  [ \$LASTEXIT -ne 0 ] && printf \" \e[1;31m[\${LASTEXIT}]\e[0m\";
  printf \" \e[33m\${PWD}\e[0m\n\";"
export PS1='> '
export PS2=' '

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
HISTSIZE=1000
HISTFILESIZE=2000
shopt -s histappend


# Aliases
# #######

alias l='ls -hal'
alias ll='ls -al'
alias ps='myps'
alias find='osxfind'
alias ssh='retitlessh'
alias less='less -Rginm'
alias funcs='declare -F'
alias func='declare -f'
alias ppath="echo \"\$PATH\" | tr ':' '\n'"
alias count_files='find -name .symform -prune -o -type f -print | wc -l'
alias rcopy='rsync -avzC'
alias reload='exec bash -l'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias homeshick="${HOME}/.homesick/repos/homeshick/home/.homeshick"


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

# Changes the terminal's and screen's titles to whatever text passed
# in (or to the previously set title if no arguments are provided).
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
    local arg name rc

    for arg in "$@"; do
	if [ "${arg:0:1}" != '-' ]; then
	    name="$arg"
	    break
	fi
    done

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

# Recursively grep files found but skipping the .svn directories. Can
# limit the scope of files to look at by providing additional find
# arguments (e.g. -name '*.cs' to look in only C# files).
function search()
{
    local find_args grep_args basedir cmd

    if [ "$1" = '-d' ]; then
        shift; basedir=$1; shift
    else
        basedir=.
    fi

    if [ "$1" = '-f' ]; then
        shift; find_args="${find_args} $1"; shift
    fi

    if [ $# -lt 1 ]; then
        echo "usage: search [-d <basedir>] [-f <find_exp>] <grep_args>" 1>&2
        return 1
    fi

    cmd="find '${basedir}' -name .svn -prune -o -type f${find_args} -print0 | \
xargs -0 grep -n --color=auto $*"
    echo "$cmd"
    eval $cmd
}
