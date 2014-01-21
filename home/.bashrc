# Environment
# ###########

export CLICOLOR=1
[ -f "${HOME}/creds/aws-${USER}.conf" ] && export AWS_CONFIG_FILE="${HOME}/creds/aws-${USER}.conf"
which emacs >/dev/null 2>&1 && export EDITOR=emacs

# If this shell is interactive, turn on programmable completion enhancements. Any completions you
# add in ~/.bash_completion are sourced last.
INTERACTIVE=false
case $- in
    *i*)
       INTERACTIVE=true
        [[ -f /etc/bash_completion ]] && . /etc/bash_completion
        [[ -f /usr/local/etc/bash_completion ]] && . /usr/local/etc/bash_completion
        [[ -f "${HOME}/Library/Python/2.7/bin/aws_completer" ]] && complete -C aws_completer aws
        [[ -f "${HOME}/.git-completion.sh" ]] && . "${HOME}/.git-completion.sh"
        [[ -f "${HOME}/.git-prompt.sh" ]] && . "${HOME}/.git-prompt.sh"
        [[ -f "${HOME}/bin/rshick" ]] && complete -F _ssh rshick
        ;;
esac

# Sets up the Bash prompt to better display the current working directory as well as exit status
# codes from failed commands.
[ -n "$SSH_CLIENT" ] && mc=36 || mc=32
SHORT_HOSTNAME=`hostname -s`
export PROMPT_COMMAND="
  LASTEXIT=\$?;
  printf \"\e[${mc}m\${USER}@\${SHORT_HOSTNAME}\";
  [ \$LASTEXIT -ne 0 ] && printf \" \e[1;31m[\${LASTEXIT}]\e[0m\";
  printf \" \e[33m\${PWD}\e[0m\$(__git_ps1 \" \e[36m(%s)\e[0m\")\n\""
export PS1='> '
export PS2=' '

export UNITYCLOUDOPS="${HOME}/work/cloud/ops"

# Prefer these directories to be at the top of the PATH.
for d in \
    '/usr/local/bin' \
    '/usr/local/sbin' \
    './node_modules/.bin' \
    './bin' \
    "${HOME}/.rvm/bin"
do
    export PATH="${d}:$(echo "$PATH" | sed -E "s#(^|:)${d}:#\1#")"
done

# Add directories to PATH if they exist.
for d in \
    '/usr/local/share/npm/bin' \
    "${HOME}/Library/Python/2.7/bin" \
    "${UNITYCLOUDOPS}/bin" \
    "${HOME}/bin"
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

qmakepath=`which qmake 2>/dev/null`
[ -n "$qmakepath" ] && export QTDIR="$(dirname "$(dirname "$qmakepath")")"


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
HISTSIZE=100000
HISTFILESIZE=100000
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
alias nohist='export HISTFILE=/dev/null'
alias sush='sudo su -s /bin/bash -'
alias wma2mp3='for f in *.wma; do ffmpeg -i "$f" -ab 128k "${f%.wma}.mp3" -ab 128K; done'

if which dircolors >/dev/null 2>&1; then
    [ -r ~/.dircolors ] && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi
# BSD's ls deals with colors without an argument
$DARWIN || alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias astyle_git='git status -s | awk '\''/^[^\?].*\.cs/{print $2}'\'' | xargs astyle --suffix=none --style=allman --indent=tab --pad-first-paren-out --keep-one-line-blocks'

if [ -d "${HOME}/work/adt" ]; then
    alias adb="${HOME}/work/adt/sdk/platform-tools/adb"
fi

if [ -e "${HOME}/.homesick/repos/homeshick/home/.homeshick" ]; then
    # Load homeshick as a function
    source $HOME/.homesick/repos/homeshick/homeshick.sh
    # let homeshick occasionally notify when it needs to be updated
    homeshick --quiet refresh
fi

if $DARWIN; then
    alias find='osxfind'
    alias netstat='osxnetstat'
    alias pstree='pstree -w'
    alias clipi='pbcopy'
    alias clipo='pbpaste'
    alias chrome='open -a /Applications/Google\ Chrome.app'
    alias vlc='open -a /Applications/VLC.app'
    alias java7='/Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java'
elif which pstree >/dev/null 2>&1; then
    alias pstree='pstree -halp'
fi


# Functions
# #########

function myps()
{
    \ps auxwwww | awk '{if(NR==1 || (tolower($0) ~ /'"$*"'/ && ! / awk .if.NR/)){print}}'
}

if $DARWIN; then

    # Automatically add in current directory if none was provided (act like GNU find).
    function osxfind()
    {
        local path="$1"
        if [ -d "$path" ]; then
            shift; \find "${path%/}" "$@"
        else
            \find . "$@"
        fi
    }

    # OS X's netstat isn't as useful as Linux's. This reports listeners correctly.
    function osxnetstat()
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
                    echo 'usage: osxnetstat [pantu]' 1>&2
                    return 1
            esac
        done

        $sudo lsof ${args[@]}
    }

    if [ -d /Applications/Unity/Unity.app ]; then
        function unity()
        {
            # this is how to open more than one unity project
            /Applications/Unity/Unity.app/Contents/MacOS/Unity -projectPath "$@" &
        }
    fi

    function dman()
    {
        open "dash://man:$@"
    }

    function dash()
    {
        open "dash://$@"
    }

fi # DARWIN

# Changes the terminal's and screen's titles to whatever text passed in (or to the previously set
# title if no arguments are provided).
export RETITLE_DEFAULT=sh
export RETITLE_PREVIOUS=sh
function retitle()
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
export -f retitle

# call retitle with ssh info and reset back to original on exit
function retitlessh()
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
export -f retitlessh

# method to use without needing curl or wget
function rawhttpget()
{
    local path

    if [ $# -ne 1 -a $# -ne 2 ]; then
        echo 'usage: httpget <host> [<path>]' 1>&2
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

function getmyip()
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
        echo 'usage: search [-d <basedir>] [-f <find_exp>] [-e <ext>[,<ext>]] <grep_args>' 1>&2
        return 1
    fi

    cmd="\find '${basedir}' \( -name .svn -o -name .git -o -name .hg \) -prune -o -type f${find_args} -print0 | \
xargs -0 grep -n --color=auto $*"
    echo "$cmd"
    eval $cmd
}

function etagsgen()
{
    local arg msg
    rm -f TAGS CSTAGS
    if [ -f bin/rails ]; then
        # requires exuberant-ctags to be installed
        echo 'generating TAGS for Rails...'
        ctags -a -e -f TAGS --tag-relative -R app lib vendor
    else
        echo 'generating TAGS for C files...'
        find . \( -name '*.h' -o -name '*.c' -o -name '*.cc' \) -print0 | xargs -0 etags -a
        if [ -f TAGS ]; then
            arg='-f CSTAGS'
        else
            msg=' (as TAGS) '
        fi
        echo "generating CSTAGS${msg}..."
        find . -name '*.cs' -print0 | xargs -0 etags -a $arg
    fi
}

function gitbranch()
{
    if [ $# -ne 1 ]; then
        echo 'usage: gitbranch <new_branch_name>' 1>&2
        return 1
    fi
    local name="$1"
    [[ "$name" == */* ]] || name="${USER}/${name}"
    git checkout -b "$name" && git push -u origin "$name"
}

function gitfullclean()
{
    local resp
    cat <<EOF 1>&2

  WARNING!  This will remove all files and directories not tracked by git.

EOF
    read -p '            To proceed, enter DELETE (in caps): ' resp
    echo
    [ "$resp" = 'DELETE' ] || return 1
    set -x
    git clean -f
    git clean -xf
    git clean -Xf
    git st --ignored -s | awk '$1 ~ /^\!\!$/ {print $2}' | xargs rm -rvf
    set +x
}

statfmt="$($DARWIN && echo '-f %u' || echo '-c %u')"
function idas()
{
    # notice uid is _not_ a local...
    uid=`id -u "$1" 2>/dev/null`
    [ -n "$uid" ] && return 0

    local curid="$(id -u)"
    local i=0
    local fn

    for fn in *; do
        local fnid="$(stat $statfmt "$fn" 2>/dev/null)"
        [ -z "$fnid" ] && continue

        # do not use the id of the running user
        if [ $curid -ne $fnid ]; then
            if [ -z "$uid" ]; then
                uid=$fnid
            elif [ $uid -ne $fnid ]; then
                echo "Files are not all owned by same user ID: ${fn} as ${fnid} others as ${uid}" 1>&2
                uid=''
                return 2
            fi
        fi

        ((i++)); [ $i -eq 10 ] && break
    done

    if [ -z "$uid" ]; then
        echo 'Unable to find alternate user ID' 1>&2
        return 3
    fi

    echo "Using user ID: ${uid}" 1>&2
    return 1
}

function runas()
{
    local uid
    idas "$1" && shift

    if [ -z "$uid" -o $# -lt 1 ]; then
        echo 'usage: runas [<user>] <cmd> [<options>...]' 1>&2
        return 1
    fi

    local tf=`mktemp /tmp/runas-XXX.sh`
    (
        [ -n "$RAILS_ENV" ] && echo "export ${sudoenv}RAILS_ENV=${RAILS_ENV}"
        [ -n "$NODE_ENV" ] && echo "export ${sudoenv}NODE_ENV=${NODE_ENV}"
        echo "cd $(pwd); exec $*"
    ) > $tf
    chmod 555 $tf

    sudo -u \#$uid $sudoenv -i $tf
    local rc=$?
    rm -f $tf
    return $rc
}

function editas()
{
    local uid
    idas "$1" && shift

    if [ -z "$uid" -o $# -ne 1 -o ! -f "$1" ]; then
        echo 'usage: editas [<user>] <file>' 1>&2
        return 1
    fi

    sudo -u \#$uid -e "$1"
}

function showansi()
{
    echo 'printf "\033[{attr};{bg};{fg}m{TEXT}\033[m" (or use \e)'
    for attr in $(seq 0 1); do
        for fg in $(seq 30 37); do
            for bg in $(seq 40 47); do
                printf "\033[$attr;${bg};${fg}m$attr;$fg;$bg\033[m "
            done
            echo
        done
    done
}

function pt()
{
    papertrail -g "$@" -f | grep -viE 'health|nagios|pingdom|localhost'
}

function pswatch()
{
    if [ $# -ne 1 ]; then
        echo 'usage pswatch <process_name>' 1>&2
        return 1
    fi
    local pid=`pgrep -o "$1"`
    if [ $? -ne 0 -o -z "$pid" ]; then
        echo "Failed to find process: $1"
        return 2
    fi
    watch pstree -pa $pid
}

# TODO: add retail!!!

# Load RVM into a shell session as a function
[[ -s "${HOME}/.rvm/scripts/rvm" ]] && source "${HOME}/.rvm/scripts/rvm"

$INTERACTIVE || return 0

# Execution
# #########

if [ -z "$SSH_CLIENT" ]; then
    # Only run an ssh-agent on a local machine...not when logged in remotely via SSH.

    . "${HOME}/.ssh/agent_env.sh" >/dev/null 2>&1
    if test -z "$SSH_AGENT_PID" || ! pgrep ssh-agent | grep -qF "$SSH_AGENT_PID"; then
        ssh-agent > "${HOME}/.ssh/agent_env.sh"
        printf 'New SSH '
        . "${HOME}/.ssh/agent_env.sh"
    fi

    if [ -f "${HOME}/.ssh/ssh_agent.keys" ]; then
        cat "${HOME}/.ssh/ssh_agent.keys" | while read key; do
            # expand tilde and other variables (e.g. $HOME)
            __expand_tilde_by_ref key
            eval "key=\"${key}\""
            ssh-add "$key" >/dev/null 2>&1
        done
    fi
fi

# On OS X, try to run cmd-key-happy to have option and command conditionally remapped.
if $DARWIN && which cmd-key-happy >/dev/null 2>&1; then
    if ! pgrep cmd-key-happy >/dev/null; then
        nohup cmd-key-happy >/dev/null 2>&1 </dev/null &
    fi
fi
