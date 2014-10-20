# Environment
# ###########

# TODO: split this up into more sharable pieces (i.e. stuff for osx, stuff for rails, stuff for git)
# TODO: reload doesn't work when root

function ihave() { \which "$@" >/dev/null 2>&1; }

export CLICOLOR=1
[ -f "${HOME}/creds/aws-${USER}.conf" ] && export AWS_CONFIG_FILE="${HOME}/creds/aws-${USER}.conf"
if ihave emacs; then
    export EDITOR=emacs
elif ihave vi; then
    export EDITOR=vi
fi

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
        [[ -f "${HOME}/.dcli-completion.sh" ]] && . "${HOME}/.dcli-completion.sh"
        [[ -f "${HOME}/bin/rshick" ]] && complete -F _ssh rshick
        if [ -d "${HOME}/.bash_completion.d" ]; then
            for s in "${HOME}"/.bash_completion.d/*.sh; do source "$s"; done
        fi
        ;;
esac

# Track if we are the superuser.
[ `id -u` -eq 0 ] && IAMROOT=true || IAMROOT=false

if [ -f "${HOME}/.pythonrc.py" ]; then
    # Point interactive Python prompt to initialize with this content.
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

[ -d /opt/unity/unitycloud-ops ] && export UNITYCLOUDOPS=/opt/unity/unitycloud-ops

if $INTERACTIVE; then
    SHORT_HOSTNAME=`hostname -s`

    if ! type -t __git_ps1 >/dev/null 2>&1; then
        # no-op this for our prompt below
        function __git_ps1()
        {
            :
        }
    fi

    if ! ihave rvm-prompt; then
        function rvm-prompt()
        {
            :
        }
    fi

    # Sets up the Bash prompt to better display the current working directory as well as exit status
    # codes from failed commands, and make superuser prompts look distinct.
    if $IAMROOT; then
        DISP_USER="\033[0;1;31m-[ ROOT ]-\033[0;1;35m ${SHORT_HOSTNAME}"
    else
        DISP_USER="${USER}@${SHORT_HOSTNAME}"
    fi

    [ -n "$SSH_CLIENT" ] && mc=36 || mc=32
    export PROMPT_COMMAND="
LASTEXIT=\$?;
printf \"\e[${mc}m\${DISP_USER}\";
[ \$LASTEXIT -ne 0 ] && printf \" \e[1;31m[\${LASTEXIT}]\e[0m\";
printf \" \e[33m\${PWD}\e[0m \e[36m(\$(rvm-prompt)\$(__git_ps1 \" %s\"))\e[0m\n\""
    export PS1='> '
    export PS2=' '

    # Prefer these directories to be at the top of the PATH.
    for d in \
        '/usr/local/bin' \
        '/usr/local/sbin' \
        './node_modules/.bin' \
        './bin' \
        "${HOME}/.rvm/bin"
    do
        [ -d "$d" ] && export PATH="${d}:$(echo "$PATH" | sed -E "s#(^|:)${d}:#\1#")"
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
fi

UNAME=`uname`
if [ "$UNAME" = 'Darwin' ]; then
    DARWIN=true
else
    DARWIN=false
fi

qmakepath=`\which qmake 2>/dev/null`
[ -n "$qmakepath" ] && export QTDIR="$(dirname "$(dirname "$qmakepath")")"

PERL_BASE="${HOME}/perl5"
[ -d "$PERL_BASE" ] && eval "$(perl -I"${PERL_BASE}/lib/perl5" -Mlocal::lib)"


# Shell Options
# #############

# notify immediately of background job state changes
set -b
shopt -s cdspell
# dirspell is a newer bash opt...
shopt -s dirspell 2>/dev/null
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
alias less='less -Rginm'
alias funcs='declare -F | grep -vF "declare -f _"'
alias func='declare -f'
alias ppath="echo \"\$PATH\" | tr ':' '\n'"
alias count_files='find -name .symform -prune -o -type f -print | wc -l'
alias rcopy='rsync -avzC'
alias reload='exec bash -l'
alias nohist='export HISTFILE=/dev/null'
alias wma2mp3='for f in *.wma; do ffmpeg -i "$f" -ab 128k "${f%.wma}.mp3" -ab 128K; done'
alias base64creds="ruby -rbase64 -e 'puts Base64.urlsafe_encode64(ARGV[0]+\":\"+ARGV[1])'"
alias reniceme='renice 10 $$'
alias rootme='sume root'
alias rcopy='rsync -avzC --exclude .hg/ --exclude node_modules/'
ihave pry && alias irb='pry'

# Sets a _GLOBAL_ $runner variable for a given command.
function localsetrunner()
{
    local d
    for d in bin script; do
        runner="./$d/$1"
        [ -x "$runner" -a ! -d "$runner" ] && return
    done
    runner="$1"
}

function localrun()
{
    localsetrunner "$1"; shift
    echo "${runner} $@"
    $runner "$@"
}

for app in bundle rails rake rspec; do
    alias ${app}="localrun ${app}"
done
unset app


if ihave dircolors; then
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

if $INTERACTIVE && test -e "${HOME}/.homesick/repos/homeshick/home/.homeshick"; then
    # Load homeshick as a function
    source $HOME/.homesick/repos/homeshick/homeshick.sh
    # let homeshick occasionally notify when it needs to be updated
    homeshick --quiet refresh
    alias hs='homeshick'
fi

if $DARWIN; then
    alias find='osxfind'
    alias netstat='osxnetstat'
    alias pstree='pstree -w'
    alias clipi='pbcopy'
    alias clipo='pbpaste'
    alias clipc='pbpaste|pbcopy'
    alias markdownitc='pbpaste|markdownit code|pbcopy'
    alias chrome='open -a /Applications/Google\ Chrome.app'
    alias vlc='open -a /Applications/VLC.app'
    alias java7='/Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java'

    f="/Applications/VMware Fusion.app/Contents/Library/vmrun"
    [ -x "$f" ] && alias vmrun="\"$f\""
else
    ihave pstree && alias pstree='pstree -halp'
    if ihave xclip; then
        alias clipi='xclip -sel clip -i'
        alias clipo='xclip -sel clip -o'
    fi
fi


# Functions
# #########

function myps()
{
    \ps auxwwww | awk '{if(NR==1 || (tolower($0) ~ /'"$*"'/ && ! / awk .if.NR/)){print}}'
}

function mywhich()
{
    local found=false
    \which "$@" 2>/dev/null && found=true
    alias "$@" 2>/dev/null && found=true
    func "$@" 2>/dev/null && found=true
    $found
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
                    echo 'usage: osxnetstat [pantu]' >&2
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

    function notify()
    {
        local msg
        if [ $# -gt 0 ]; then
            msg="$@"
        else
            msg='Action is complete'
        fi
        terminal-notifier -sound default -message "$msg"
    }

fi # DARWIN

# Changes the terminal's and screen's titles to whatever text passed in (or to the previously set
# title if no arguments are provided).
export RETITLE_DEFAULT=sh
export RETITLE_PREVIOUS=sh
function retitle()
{
    [ -n "$SSH_CLIENT" ] && return 0
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
        echo 'usage: httpget <host> [<path>]' >&2
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
if ihave curl; then
    httpget='curl -k'
else
    httpget='wget -q --no-check-certificate -O -'
fi

function getmyip()
{
    local scheme
    if [ $# -eq 0 ]; then
        echo "${httpget} http://curlmyip.com"
        $httpget http://curlmyip.com
    else
        echo "${httpget} https://ip.appspot.com"
        $httpget https://ip.appspot.com
    fi
}

if ihave hg; then
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
fi

# Recursively grep files found but skipping the .svn directories. Can limit the scope of files to
# look at by providing additional find arguments (e.g. -name '*.cs' to look in only C# files).
function search()
{
    local find_args basedir cmd ext extprefix

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
        echo 'usage: search [-d <basedir>] [-f <find_exp>] [-e <ext>[,<ext>]] { print | <grep_args> }' >&2
        return 1
    fi

    cmd="\find '${basedir}' \( -name .svn -o -name .git -o -name .hg \) -prune -o \
-not -name '*~' -type f${find_args} -print"
    [ "$*" = 'print' ] || cmd="${cmd}0 | xargs -0 grep -n -I --color=auto $@"
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

if ihave git; then
    function gitsetbranchname()
    {
        branch_name="$(git symbolic-ref HEAD 2>/dev/null)" ||
        branch_name="(unnamed branch)"     # detached HEAD
        branch_name=${branch_name##refs/heads/}
    }

    function gitbranch()
    {
        local push
        if [ "$1" = '-p' ]; then
            shift
            push='git push'
        else
            push=true
        fi
        if [ $# -ne 1 ]; then
            echo 'usage: gitbranch [-p] <new_branch_name>' >&2
            return 1
        fi
        local name="$1"
        [[ "$name" == */* ]] || name="${USER}/${name}"
        gitsetbranchname
        if [[ "$branch_name" != 'release/'* ]]; then
            read -p "Branch from ${branch_name}? [y|n] "
            [ "$REPLY" = 'y' ] || return 1
        fi
        git checkout -b "$name" && $push -u origin "$name"
    }

    function gitfullclean()
    {
        local resp
        cat <<EOF >&2

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
fi

function id2name()
{
    if $DARWIN; then
        dscl . -search /Users UniqueID "$1" | sed -n 's/^\([^[:space:]]*\).*$/\1/p;q'
    else
        getent passwd "$1" | cut -d: -f1
    fi
}

statfmt="$($DARWIN && echo '-f %u' || echo '-c %u')"
function idas()
{
    # notice uid and uidname are _not_ locals...thus, they are outputs from this call
    uidname="$1"
    uid=`id -u "$uidname" 2>/dev/null`
    [ -n "$uid" ] && return 0

    local curid="$(id -u)"
    local i=0
    local fn
    local dn

    if [ -e "$uidname" ]; then
        # if passed a file/directory, use that location for determining a uid
        [ -d "$uidname" ] && dn="$uidname" || dn="$(dirname "$uidname")"
        uidname=''
    else
        dn='.'
    fi

    for fn in "$dn"/*; do
        local fnid="$(stat $statfmt "$fn" 2>/dev/null)"
        [ -z "$fnid" ] && continue

        # do not use the id of the running user
        if [ $curid -ne $fnid ]; then
            if [ -z "$uid" ]; then
                uid=$fnid
            elif [ $uid -ne $fnid ]; then
                echo "Files are not all owned by same user ID: ${fn} as ${fnid} others as ${uid}" >&2
                uid=''
                return 2
            fi
        fi

        ((i++)); [ $i -eq 10 ] && break
    done

    if [ -z "$uid" ]; then
        echo 'Unable to find alternate user ID' >&2
        return 3
    fi

    uidname="$(id2name "$uid")"
    echo "Using user ID: ${uid} (${uidname})" >&2
    return 1
}

function editas()
{
    local uid uidname
    idas "$1" && shift

    if [ -z "$uid" -o $# -ne 1 -o ! -f "$1" ]; then
        echo 'usage: editas [<user>] <file>' >&2
        return 1
    fi

    sudo -u \#$uid -e "$1"
}

function runas()
{
    local uid uidname owner initfn irbrcfn evar evar_val
    idas "$1" && shift

    if [ -z "$uid" -o $# -lt 1 ]; then
        echo 'usage: runas [<user>] <cmd> [<options>...]' >&2
        return 1
    fi

    if [ "$USER" = "$uidname" ]; then
        echo "Already $USER" >&2
        return 2
    fi

    if $DARWIN; then
        owner="$(id2name "$(stat -f %u "${BASH_SOURCE[0]}")")"
    else
        owner="$(stat -c %U "${BASH_SOURCE[0]}")"
    fi

    initfn="${HOME}/.sume_${uidname}.sh"
    echo '#!/bin/sh' >"$initfn"
    for evar in SSH_CLIENT SSH_AUTH_SOCK RAILS_ENV NODE_ENV; do
        evar_val="$(eval echo \$$evar)"
        [ -n "$evar_val" ] && echo "export ${evar}=\"${evar_val}\"" >>"$initfn"
    done

    irbrcfn="${HOME}/.irbrc"
    if [ -f "$irbrcfn" ]; then
        # load the user's own IRB rc file for configuration when launching irb or rails console
        echo "export IRBRC=\"${irbrcfn}\"" >>"$initfn"
    fi

    cat <<EOF >>"$initfn"
export SUME_PWD="${PWD}"
cd "${PWD}"
exec $*
EOF

    chmod 755 "$initfn"
    echo "Switching user from ${USER} to ${uidname}..."
    sudo -u "$uidname" -i "$initfn"
}
# If invoked from sume (see above), go back to original path...
if [ -n "$SUME_PWD" ]; then
    cd "$SUME_PWD"
    unset SUME_PWD
fi

function sume()
{
    runas "$1" /bin/bash --rcfile "${BASH_SOURCE[0]}" -i
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

if ihave papertrail; then
    function pt()
    {
        papertrail -g "$@" -f | grep -viE 'health|nagios|pingdom|localhost'
    }
fi

function pswatch()
{
    if [ $# -ne 1 ]; then
        echo 'usage pswatch <process_name>' >&2
        return 1
    fi
    local pid=`pgrep -o "$1"`
    if [ $? -ne 0 -o -z "$pid" ]; then
        echo "Failed to find process: $1"
        return 2
    fi
    watch pstree -pa $pid
}

if ihave dcli; then
    function dcli_get_all_app()
    {
        if [ $# -ne 1 ]; then
            echo 'usage dcli_get_all_app <app_id>' >&2
            return 1
        fi
        local cmd
        for cmd in app sources interstitial impressions installs; do
            echo; echo ---
            printf "\"${cmd}\": "
            dcli get_$cmd $1 || break
        done
    }
fi

# Convert stdin to stdout so that it is pastable as markdown block snippets.
function markdownit()
{
    local prefix
    case "$1" in
        code) prefix='    ';;
        block|quote) prefix='> ';;
        *)
            echo 'markdownit { code | block }' >&2
            return 1
            ;;
    esac
    echo; sed "s/^/${prefix}/"
}

# Remove empty lines and bullets from start of lines (primarily for cleaning out copy from evernote).
function clipstrip()
{
    clipo | sed '/^ *$/d;s/^ *[\*\-\#] *//' | clipi
}

if [ -d /proc ]; then
    function penv()
    {
        if $IAMROOT; then
            tr '\0' '\n' < /proc/$1/environ | sort
        else
            sudo cat /proc/$1/environ | tr '\0' '\n' | sort
        fi
    }
elif $DARWIN; then
    function penv()
    {
        if $IAMROOT; then
            \ps -wwwwE $1
        else
            sudo ps -wwwwE $1
        fi
    }
fi

# Tail a file with a regular expression that highlights any matches from the tail output.
HILIGHT=`echo -e '\033[30m\033[43m'`
NORMAL=`echo -e '\033[0m'`
function retail()
{
    local targs
    while [ $# -gt 1 ]; do
        targs="${targs} $1"
        shift
    done

    if [ $# -ne 1 ]; then
        echo 'usage: retail [<tail_arguments>] <regexp>' >&2
        return 1
    fi

    exec tail $targs | awk '{if ($0 ~ /'"${1}"'/) {print "'"${HILIGHT}"'" $0 "'"${NORMAL}"'"} else {print}}'
}

# Tail a file in the background while another process runs in the foreground, killing off the tail
# when the foreground process is done.
function tailrun()
{
    local tpid cmdrc

    if [ $# -lt 2 -o ! -f "$1" ]; then
        echo 'usage: tailrun <file> <cmd> [<options>...]' >&2
        return 1
    fi

    tail -Fn0 "$1" &
    tpid=$!
    shift

    "$@"
    cmdrc=$?
    kill $tpid

    return $cmdrc
}

# If the last argument looks like running a specific test by line number, tail the test log,
# otherwise, run it like normal
function rsp()
{
    localsetrunner rspec
    if [[ -f log/test.log && "${@: -1}" =~ :[0-9]+$ ]]; then
        tailrun log/test.log "$runner" "$@"
    else
        $runner --order defined "$@"
    fi
}

function httpfileserver()
{
    if [ $# -ne 2 ]; then
        echo 'usage: httpfileserver <port> <directory>' >&2
        return 1
    fi

    (
        cd "$2" &&
        python -m SimpleHTTPServer $1
    )
}

if ihave bundle; then
    function bundle-use-local()
    {
        if [ $# -ne 2 ]; then
            echo 'usage: bundle-use-local <gemname> <local_path>' >&2
            return 1
        fi
        bundle config --local "local.$1" "$2" && bundle update "$1" && bundle clean --force
    }
    function bundle-use-remote()
    {
        if [ $# -ne 1 ]; then
            echo 'usage: bundle-use-remote <gemname>' >&2
            return 1
        fi
        bundle config --delete "local.$1" && bundle update "$1" && bundle clean --force
    }
    function bundle-install-clean()
    {
        bundle install && bundle clean --force
    }
fi

# Load RVM into a shell session as a function
for f in \
    "${HOME}/.rvm/scripts/rvm" \
    '/etc/profile.d/rvm.sh'
do
    if [ -s "$f" ]; then
        source "$f"

        function rvm_remember()
        {
            local str v g
            str="$(rvm info | sed -n '/^ruby-/{s/^\(ruby-[^@:]*\)@*\([^:]*\).*$/v="\1";g="\2"/p;q;}')"
            eval "$str"
            if [ -n "$v" ]; then
                echo "$v -> .ruby-version"
                echo "$v" >.ruby-version
            fi
            if [ -n "$g" ]; then
                echo "$g -> .ruby-gemset"
                echo "$g" >.ruby-gemset
            fi
        }

        function rvm_gemset_from_git()
        {
            local name
            name="$(git remote -v | sed 's/^.*\/\(.*\).git.*$/\1/;q')"
            if [ -z "$name" ]; then
                echo 'Unable to determine git remote name' >&2
                return 1
            fi
            echo "${name} -> .ruby-gemset"
            echo "${name}" >.ruby-gemset
        }

        break
    fi
done

$INTERACTIVE || return 0

# Execution
# #########

if ! $IAMROOT && [ -d "${HOME}/bin" -a ! -x "${HOME}/bin/spot" ]; then
    # File content search tool
    # TODO make this work with httpget! curl isn't always installed (e.g. ubuntu)
    echo 'Downloading "spot" search tool to bin...'
    curl -sfSL https://raw.github.com/guille/spot/master/spot.sh -o "${HOME}/bin/spot" && \
        chmod +x "${HOME}/bin/spot"
fi

if test -z "$SSH_CLIENT" && ! $IAMROOT; then
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
if $DARWIN && ihave cmd-key-happy; then
    if ! pgrep cmd-key-happy >/dev/null; then
        nohup cmd-key-happy >/dev/null 2>&1 </dev/null &
    fi
fi

if test -e /etc/ec2_version && ihave ec2tags; then
    # Some EC2 instances will use tags to indicate environment settings to web frontends.
    EC2_ENV="$(ec2tags env 2>/dev/null || :)"
    if [ -n "$EC2_ENV" ]; then
        export RAILS_ENV="$EC2_ENV"
        export NODE_ENV="$EC2_ENV"
        echo "Set Rails and Node environment to ${EC2_ENV}"
    else
        unset EC2_ENV
    fi
fi

if ! $IAMROOT; then
    # link in ssh keys for this host
    for src in "${HOME}/.ssh/id_"*"${SHORT_HOSTNAME}"; do
        if [ -f "$src" ]; then
            dst="${src%_${SHORT_HOSTNAME}}"
            [ -L "$dst" ] && rm -f "$dst"
            ln -s "$src" "$dst"
        fi
    done
    unset src
    unset dst
fi

# these override actual tools, so place them at the very end...
alias ps='myps'
alias which='mywhich'
alias ssh='retitlessh'
if ihave discard; then
    alias rm=discard
fi
