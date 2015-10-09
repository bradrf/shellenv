# Environment
# ###########

# TODO: split this up into more sharable pieces (i.e. stuff for osx, stuff for rails, stuff for git)
# TODO: reload doesn't work when root
# TODO: no-op downloading (and other options touching "home") when sume
# TODO: make bundle function to prompt if there is no rvm gemset established

function ihave() { \which "$@" >/dev/null 2>&1; }

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

export NPM_PACKAGES="${HOME}/.npm-packages"
if [ ! -d "$NPM_PACKAGES" ]; then
    mkdir -p "$NPM_PACKAGES"
    echo "prefix = ${NPM_PACKAGES}" >> ~/.npmrc
fi

# Add directories to PATH if they exist.
for d in \
    '/usr/local/android-studio/bin' \
    '/usr/local/heroku/bin' \
    '/usr/local/share/npm/bin' \
    "${NPM_PACKAGES}/bin" \
    "${HOME}/Library/Python/2.7/bin" \
    "${HOME}/.gem/ruby/"**"/bin" \
    "${HOME}/bin" \
    "${HOME}/.local/bin"
do
    if [ -d "$d" ]; then
        echo "$PATH" | grep -qE ":${d}(:|\$)" || export PATH="${PATH}:$d"
    fi
done

GOOGLE_CLOUD_SDK='/usr/local/src/google-cloud-sdk'
if [ -d "$GOOGLE_CLOUD_SDK" ]; then
    . "${GOOGLE_CLOUD_SDK}/path.bash.inc"
else
    unset GOOGLE_CLOUD_SDK
fi

INTERACTIVE=false
case $- in
    *i*)
        INTERACTIVE=true
        [ -f /etc/bash_completion ] && . /etc/bash_completion
        [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
        [ -n "$GOOGLE_CLOUD_SDK" ] && . "${GOOGLE_CLOUD_SDK}/completion.bash.inc"
        ihave aws_completer && complete -C aws_completer aws
        ihave rshick && complete -F _ssh rshick
        if [ -d "${HOME}/.bash_completion.d" ]; then
            for s in "${HOME}"/.bash_completion.d/*.sh; do source "$s"; done
        fi
        if [ -d "${HOME}/lib/bash" ]; then
            for s in "${HOME}/lib/bash"/*.sh; do source "$s"; done
        fi
        unset s
        ;;
esac

# Track if we are the superuser.
[ `id -u` -eq 0 ] && IAMROOT=true || IAMROOT=false

# Track if we are ourselves (i.e. not root and not switched from another user via sume).
! $IAMROOT && test -z "$SUDO_USER" && IAMME=true || IAMME=false

# Build SSH configuration file (NOTE: this only happens once--to update, delete the current file).
if $IAMME && test -d "${HOME}/.ssh"; then
    if [ ! -f "${HOME}/.ssh/config" ]; then
        shopt -s nullglob # empty list if no match
        cfns=("${HOME}/.ssh/config_"*)
        (
            awk '/^# DEFAULTS ##*$/{exit}1' "${HOME}/.ssh/configbase"
            [ ${#cfns[@]} -gt 0 ] && cat "${cfns[@]}"
            awk '/^# DEFAULTS/,0' "${HOME}/.ssh/configbase"
        ) > "${HOME}/.ssh/config"
        unset cfns
        shopt -u nullglob
    fi
fi

if [ -f "${HOME}/.pythonrc.py" ]; then
    # Point interactive Python prompt to initialize with this content.
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

[ -d "${HOME}/lib/python" ] && export PYTHONPATH="${HOME}/lib/python"

[ -d "${HOME}/lib/ruby" ] && export RUBYLIB="${HOME}/lib/ruby"

aws_fn="${HOME}/creds/aws-${USER}.conf"
if [ -f "$aws_fn" ]; then
    export AWS_CONFIG_FILE="$aws_fn"
    export BOTO_CONFIG="$aws_fn"
    mkdir -p "${HOME}/.aws"
    ln -sf "$aws_fn" "${HOME}/.aws/credentials"
fi
unset aws_fn

[ -f "${HOME}/creds/creds.sh" ] && . "${HOME}/creds/creds.sh"

if $INTERACTIVE; then
    export CLICOLOR=1

    if ihave emacs; then
        export ALTERNATE_EDITOR=emacs
    elif ihave vi; then
        export ALTERNATE_EDITOR=vi
    elif ihave pico; then
        export ALTERNATE_EDITOR=pico
    fi

    if test -e "${HOME}/bin/climacs" && ihave emacsclient; then
        export EDITOR="${HOME}/bin/climacs"
    elif [ -n "${ALTERNATE_EDITOR}" ]; then
        export EDITOR="${ALTERNATE_EDITOR}"
    fi

    export SHORT_HOSTNAME=`hostname -s`

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
[ -n \"\$FLASH\" ] && printf \"\e[1;31m\${FLASH}\e[0m\";
printf \"\e[${mc}m\${DISP_USER}\";
[ \$LASTEXIT -ne 0 ] && printf \" \e[1;31m[\${LASTEXIT}]\e[0m\";
printf \" \e[33m\${PWD}\e[0m \e[36m(\$(rvm-prompt)\$(__git_ps1 \" %s\"))\e[0m\n\""
    export PS1='> '
    export PS2=' '
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

# FIXME: This is a work-around for Wireshark (invalid unclassed pointer in cast to 'GObject')
#export LIBOVERLAY_SCROLLBAR=0


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

HISTCONTROL=ignoredups:erasedups
HISTSIZE=100000
HISTFILESIZE=100000
shopt -s histappend


# Aliases
# #######

# rename bashmarks' listing function so it doesn't clash with file listing alias below
if declare -F l >/dev/null 2>&1; then
    eval "$(echo "b()"; declare -f l | tail -n +2)"
    unset l
fi

alias l='ls -hal'
alias ll='ls -al'
alias less='less -Rginm'
alias lesstrunc='less -S'
alias funcs='declare -F | grep -vF "declare -f _"'
alias func='declare -f'
alias ppath="echo \"\$PATH\" | tr ':' '\n'"
alias rcopy='rsync -avzC'
alias reload='exec bash -l'
alias nohist='export HISTFILE=/dev/null'
alias wma2mp3='for f in *.wma; do ffmpeg -i "$f" -ab 128k "${f%.wma}.mp3" -ab 128K; done'
alias base64creds="ruby -rbase64 -e 'puts Base64.urlsafe_encode64(ARGV[0]+\":\"+ARGV[1])'"
alias reniceme='renice 10 $$'
alias rootme='sudo -s'
alias rcopy='rsync -avzC --exclude .hg/ --exclude node_modules/'
alias zipdir='zip -9 -r --exclude=*.svn* --exclude=*.git* --exclude=*.DS_Store* --exclude=*~'
alias ghist='history | grep'

ihave pry && alias irb='pry'
ihave docker && alias sd='sudo docker'

if ihave aws; then
    alias s3='aws s3'
    alias ec2='aws ec2'
    # if only "Output" then it's the wrong region!, should create a func for this?
    alias ec2logs='ec2 --output text get-console-output --instance-id'
fi

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
    echo "${runner} $@" >&2
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
elif [ -d "${HOME}/Android" ]; then
    alias adb="${HOME}/Android/Sdk/platform-tools/adb"
fi

if $INTERACTIVE && test -e "${HOME}/.homesick/repos/homeshick/homeshick.sh"; then
    # Load homeshick as a function
    source $HOME/.homesick/repos/homeshick/homeshick.sh
    # let homeshick occasionally notify when it needs to be updated
    homeshick -qb refresh
    alias hs='homeshick'
fi

if $DARWIN; then
    alias find='osxfind'
    alias netstat='osxnetstat'
    alias pstree='pstree -w'
    alias service='osxservice'
    alias clipi='pbcopy'
    alias clipo='pbpaste'
    alias clipc='pbpaste|pbcopy'
    alias markdownitc='pbpaste|markdownit code|pbcopy'
    alias chrome='open -a /Applications/Google\ Chrome.app'
    alias vlc='open -a /Applications/VLC.app'
    alias javare='/Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java'
    alias eject='hdiutil eject'

    f="/Applications/VMware Fusion.app/Contents/Library/vmrun"
    [ -x "$f" ] && alias vmrun="\"$f\""
    unset f
else
    ihave xdg-open && alias open='xdg-open'
    ihave pstree && alias pstree='pstree -halp'
    if ihave xclip; then
        alias clipi='xclip -sel clip -i'
        alias clipo='xclip -sel clip -o'
    fi
    if ihave udisksctl; then
        function eject()
        {
            if [ $# -ne 1 ]; then
                echo 'eject <device> (e.g. eject sdb2)' >&2
                return 1
            fi
            udisksctl unmount --block-device /dev/$1 && udisksctl power-off --block-device /dev/${1::-1} && \
                sudo rmmod uas usb_storage nls_utf8 hfsplus
        }
    fi
fi

alias eclipo='eval $(clipo)'


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
            local last args=( "$@" )
            last="${@: -1}"
            unset args[${#args[@]}-1]
            if [ -d "$last" ]; then
                # this is how to open more than one unity project
                args+=(-projectPath "$last")
            else
                args+=("$last")
            fi
            tailrun "${HOME}/Library/Logs/Unity/Editor.log" /Applications/Unity/Unity.app/Contents/MacOS/Unity "${args[@]}"
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

function caseit()
{
    local d=$1; shift
    local u='[:upper:]'
    local l='[:lower:]'
    local tr
    [ "$d" = 'up' ] && tr="tr $l $u" || tr="tr $u $l"
    [ $# -gt 0 ] && echo "$*" | $tr || $tr
}
alias downcase='caseit down'
alias upcase='caseit up'

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

# mechanism for copying files from a remote system that requires different privileges
# NOTE: the output of this call is tar and is expected to be piped or captured; examples:
#       sshtar foo-host /bar/baz/foozy > foozy.tgz
#       sshtar foo-host /var/baz/foozy | tar zx
function sshtar()
{
    local rhost rdir rbase rdirp
    if [ $# -ne 2 ]; then
        echo 'usage: sshtar <remote_host> <remote_directory>' >&2
        return 1
    fi
    rhost="$1"
    rdir="$2"
    rbase="`basename "$rdir"`"
    rdirp="`dirname "$rdir"`"
    ssh "$rhost" "sudo tar -C '${rdirp}' -cz '${rbase}'"
}

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

function download()
{
    local bn hfn
    if [ $# -ne 1 ]; then
        echo 'usage: download <url>' >&2
        return 1
    fi
    if ! ihave curl; then
        echo 'todo: need to add support for wget' >&2
        return 2
    fi
    bn="$(basename "$1")"
    hfn="${bn}.headers"
    # use etags to only download if changed from server
    curl --dump-header "$hfn" \
        --header "If-None-Match: $(sed 's/ETag: \(.*\)/\1/p;d' "$hfn" 2>/dev/null)" \
        -o "$bn" -sL -w "Response: %{http_code}\nDownloaded: %{size_download} bytes\n" "$1"
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

function getservercert()
{
    openssl x509 -in <(openssl s_client -connect $1:443 -prexit 2>/dev/null) -text -noout
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

    function gitclean()
    {
        git clean -nX
        echo
        read -p 'Remove? (y|N): ' resp
        echo
        [ "$resp" == 'y' ] || return 1
        git clean -fX
    }

    function gitcheck() {
        local rc=0

        # Update the index
        git update-index -q --ignore-submodules --refresh

        if ! git diff-files --quiet --ignore-submodules --; then
            echo -e "\nUnstaged changes:" >&2
            git diff-files --name-status -r --ignore-submodules -- >&2
            (( rc++ ))
        fi

        if ! git diff-index --cached --quiet HEAD --ignore-submodules --; then
            echo -e "\nIndex contains uncommitted changes:" >&2
            git diff-index --cached --name-status -r --ignore-submodules HEAD -- >&2
            (( rc++ ))
        fi

        gitsetbranchname
        local unpushed=`git diff --numstat --cached "origin/${branch_name}"`
        if [ -n "$unpushed" ]; then
            echo -e "\nFiles waiting to be pushed:\n$unpushed" >&2
            (( rc++ ))
        fi

        local untracked=`git ls-files . --exclude-standard --others`
        if [ -n "$untracked" ]; then
            echo -e "\nUntracked files:\n$untracked" >&2
            (( rc++ ))
        fi

        return $rc
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
    local uid

    if [ $# -eq 2 ]; then
        uid="$1"
        shift
    fi

    if [ $# -ne 1 ]; then
        echo 'usage: editas [<user>] <file>' >&2
        return 1
    fi

    [ -z "$uid" ] && uid="#$(stat $statfmt "$1")"

    sudo -u "$uid" -e "$1"
}

# FIXME:
#   /home/brad/.irbrc:35:in `initialize': Permission denied @ rb_sysopen - /home/brad/.irbhst (Errno::EACCES)
#           from /home/brad/.irbrc:35:in `open'
#           from /home/brad/.irbrc:35:in `block in <top (required)>'
#   /usr/local/rvm/scripts/irbrc.rb:32:in `initialize': Permission denied @ rb_sysopen - /home/brad/.irb-history (Errno::EACCES)
#           from /usr/local/rvm/scripts/irbrc.rb:32:in `open'
#           from /usr/local/rvm/scripts/irbrc.rb:32:in `block in <top (required)>
function sume()
{
    local uid uidname
    idas "$1" && shift

    if [ -z "$uid" ]; then
        echo 'usage: sume [<user>]' >&2
        return 1
    fi

    if [ "$USER" = "$uidname" ]; then
        echo "Already $USER" >&2
        return 2
    fi

    sudo -u "$uidname" -s
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
        local pid sudo
        $IAMROOT || sudo=sudo
        for pid in `pgrep -f "$@"`; do
            cat <<EOF
-- ${pid} --------------------------------------------------------------------
EOF
            $sudo cat "/proc/${pid}/environ" | tr '\0' '\n' | sort
        done
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

# Tail a file truncating long lines to the width of the terminal.
# Passes all commands to tail. Alows for piping in to less (or see lesstrunc alias above).
function tailtrunc()
{
    tail "$@" | cut -c -$COLUMNS
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

if ihave pip; then
    function pip_upgrade()
    {
        pip list --user --outdated | cut -d' ' -f1 | while read; do
            echo "$REPLY"
            pip install --user -U "$REPLY"
        done
    }
fi

ihave pygmentize && PRETTYCMD='python -mjson.tool | pygmentize -l json' || PRETTYCMD='python -mjson.tool'
alias prettyjson=$PRETTYCMD

INC_COUNTS=()
# provide incrementing prefix values (useful for generating file names)
function inc()
{
    local count
    if [ $# -ne 1 ]; then
        echo 'usage: inc <prefix>' >&2
        return 1;
    fi
    count=${INC_COUNTS[$1]}
    [ -z "$count" ] && count=1
    INC_COUNTS[$1]=`expr $count + 1`
    echo "${1}-${count}"
}

function tohtml()
{
    if [ $# -ne 1 ]; then
        echo 'usage: tohtml <title>' >&2
        return 1
    fi
    tee >(aha -t "$1" > "$1.html")
}

function calc()
{
    awk 'BEGIN {print '"$*"'}'
}

# converts 1024-based MB/s to Mbits/s
function toMbps()
{
    calc "$1 * 8388608 / 1000000"
}

if ihave aws; then
    function awsenv()
    {
        local v
        if [ -n "$1" ]; then
            for v in FLASH NODE_ENV AWS_PROFILE AWS_REGION; do unset $v; done
            case "$1" in
                dev*)
                    export AWS_ENV="${USER}-development"
                    export AWS_REGION=us-west-2
                    ;;
                andy*)
                    export AWS_ENV="andy"
                    FLASH="{ $AWS_ENV } "
                    export NODE_ENV="$AWS_ENV"
                    export AWS_REGION=us-west-1
                    ;;
                stag*)
                    export AWS_ENV='staging'
                    FLASH="{ $AWS_ENV } "
                    export NODE_ENV="$AWS_ENV"
                    export AWS_REGION=us-west-1
                    ;;
                gw)
                    export AWS_ENV='gw'
                    FLASH="{ $AWS_ENV } "
                    export AWS_PROFILE=$AWS_ENV
                    ;;
                *)
                    echo "Unknown AWS environment value: $1" >&2
                    return 1
            esac
            AWS_ENV_PREFIX="${AWS_ENV}-"
            [ -n "$AWS_REGION" ] && export AWS_DEFAULT_REGION="$AWS_REGION" || unset AWS_DEFAULT_REGION
            [ -n "$AWS_PROFILE" ] && export AWS_DEFAULT_PROFILE="$AWS_PROFILE" || unset AWS_DEFAULT_PROFILE
        fi

        echo "${AWS_ENV}:"
        env | grep -E '^(AWS|BOTO)' | sort | sed 's/^/  /'
    }

    function ec2din()
    {
        if [ $# -eq 2 ]; then
            aws --region "$1" ec2 --output text describe-instances --instance-id "$2"
            return
        fi
        saws -t instances "$@"| sort | column -t
    }

    function awsdnsdump()
    {
        saws -t names "$@" | sort | column -t
    }

    function awslogs()
    {
        local group cmd stream start
        if [ "$1" = '-s' ]; then
            # avoid missing events and only look up to this many hours ago...
            start=$(expr `date -d "$1 hours ago" +%s` \* 1000)
            echo "Starting time: $start"
            start="--start-time $start"
            shift; shift;
        fi
        if [ $# -lt 1 ]; then
            ( echo 'usage: awslogs [-s <start_hours_ago>] <group_name> [<stream> | latest]';
                aws logs describe-log-groups --output text --query 'logGroups[*].[storedBytes,logGroupName]' ) >&2
            return 1
        fi
        group="$1"
        cmd="aws logs describe-log-streams --output text --log-group-name $group \
--query logStreams[*].[creationTime,logStreamName]"
        echo $cmd
        if [ -z "$2" ]; then
            $cmd | sort | awk '{ print strftime("%Y-%m-%dT%H:%M:%S%z", $1/1000) " " $2}'
            return
        fi
        if [ "$2" = 'latest' ]; then
            stream=`$cmd | sort | awk 'END { print $2 }'`
        else
            stream="$2"
        fi
        aws logs get-log-events --log-group-name $group --log-stream-name $stream --output text \
            $start --query 'events[*].[timestamp,message]' | \
            awk '/^[[:digit:]]+/ { print strftime("%Y-%m-%dT%H:%M:%S%z",$1/1000) " " $0 } !/^[[:digit:]]+/ { print $0 }'
    }

    function sqsq()
    {
        aws sqs get-queue-url --queue-name "${AWS_ENV_PREFIX}$1" --output text
    }

    function sqsls()
    {
        local q
        local attrs='ApproximateNumberOfMessages ApproximateNumberOfMessagesNotVisible ApproximateNumberOfMessagesDelayed'
        if [ -n "$1" ]; then
            q=`sqsq $1`
            echo "$q"
            aws sqs get-queue-attributes --queue-url "$q" --query 'Attributes' --attribute-names $attrs
        else
            local q s
            for q in `aws sqs list-queues --output text --query 'QueueUrls[*]'`; do
                echo "$q" | grep -qF "${AWS_ENV_PREFIX}" || continue
                s="$(basename "$q")"
                (aws sqs get-queue-attributes --queue-url "$q" --query 'Attributes' --attribute-names $attrs | \
                    awk -v q="${s}" '!/: "0"|^[{}]$/ {print q,$0}' &) | sort
                # above will show only non-zero counts
            done
        fi
    }

    function sqspeek()
    {
        local count
        [ -n "$2" ] && count=$2 || count=1
        aws sqs receive-message --queue-url `sqsq $1` --output text --query 'Messages[*].[Body]' \
            --max-number-of-messages $count --visibility-timeout 1 | prettyjson
    }

    function sqspop()
    {
        local url fn handle
        url=`sqsq $1`
        fn=`tempfile`
        aws sqs receive-message --queue-url "$url" --query 'Messages' | tee "$fn"
        handle=`cat "$fn" | awk '/ReceiptHandle/ {gsub(/[,"]/,""); print $2}'`
        aws sqs delete-message --queue-url "$url" --receipt-handle "$handle"
    }

    function sqspush()
    {
        if [ $# -ne 2 ]; then
            echo 'usage: sqspush <queue_name> <body>' >&2
            return 1;
        fi
        aws sqs send-message --queue-url `sqsq $1` --message-body "$2"
    }

    function sqspurge()
    {
        if [ $# -ne 1 ]; then
            echo 'usage: sqspurge <queue_name>' >&2
            return 1;
        fi
        aws sqs purge-queue --queue-url `sqsq $1`
    }
fi

if ihave bundle; then
    function bundle-use-local()
    {
        if [ $# -ne 2 ]; then
            echo 'usage: bundle-use-local <gemname> <local_path>' >&2
            return 1
        fi

        local bn tf

        bn=`cd "$2" && gitsetbranchname && echo "$branch_name"`
        sed -i .prev '/'"$1"'/ { n; n; s|^\( *\).*$|\1branch: "'"$bn"'"|; }' Gemfile

        bundle config --local "local.$1" "$2" && bundle update "$1" && bundle clean --force
    }

    function bundle-use-remote()
    {
        if [ $# -ne 1 ]; then
            echo 'usage: bundle-use-remote <gemname>' >&2
            return 1
        fi

        git co Gemfile && bundle config --delete "local.$1" && bundle update "$1" && bundle clean --force
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

        # usage: rvm_remember [<gemset_name>]
        function rvm_remember()
        {
            local str v g
            str="$(rvm info | sed -n '/^ruby-/{s/^\(ruby-[^@:]*\)@*\([^:]*\).*$/v="\1";g="\2"/p;q;}')"
            eval "$str"
            if [ -n "$v" ]; then
                echo "$v -> .ruby-version"
                echo "$v" >.ruby-version
            fi
            [ $# -gt 0 ] && g="$1"
            [ -n "$g" ] || g="$(git remote -v | sed 's/^.*\/\(.*\).git.*$/\1/;q')"
            if [ -n "$g" ]; then
                echo "$g -> .ruby-gemset"
                echo "$g" >.ruby-gemset
            fi
        }

        break
    fi
done


# Other Configuration
# ###################

shopt -s nullglob # empty list if no match
for f in "${HOME}/.bashrc_"*; do source "$f"; done
unset f
shopt -u nullglob


########################
$INTERACTIVE || return 0
########################


# Execution
# #########

if ! $IAMROOT && [ ! -f "${HOME}/lib/bash/bashmarks.sh" ]; then
    mkdir -p "${HOME}/lib/bash"
    echo 'Downloading "bashmarks"...'
    curl -sfSL https://raw.githubusercontent.com/huyng/bashmarks/master/bashmarks.sh \
         -o "${HOME}/lib/bash/bashmarks.sh"
fi

# set default env to development
ihave aws && test -z "$AWS_ENV_PREFIX" && awsenv development >/dev/null

if ! $IAMROOT && [ -d "${HOME}/bin" -a ! -x "${HOME}/bin/spot" ]; then
    # File content search tool
    # TODO make this work with httpget! curl isn't always installed (e.g. ubuntu)
    echo 'Downloading "spot" search tool to bin...'
    curl -sfSL https://raw.githubusercontent.com/rauchg/spot/master/spot.sh -o "${HOME}/bin/spot" && \
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

    # NOTE: each entry MUST be ended with a newline (esp the last line!)
    if [ -f "${HOME}/.ssh/ssh_agent.keys" ]; then
        cat "${HOME}/.ssh/ssh_agent.keys" | while read key; do
            # expand tilde and other variables (e.g. $HOME)
            __expand_tilde_by_ref key
            eval "key=\"${key}\""
            ssh-add "$key" >/dev/null 2>&1
        done
    fi
fi

mkdir -p "${HOME}/.ssh/cm_sockets" # used by ssh config ControlPath

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
        # TODO: only set if rails and node are installed (log appropriately)
        awsenv "$EC2_ENV"
    else
        unset EC2_ENV
    fi
fi

if ! $IAMROOT; then
    # link in ssh keys for this host
    for src in "${HOME}/.ssh/id_"*"${SHORT_HOSTNAME}"; do
        if [ -f "$src" ]; then
            dst="${src%_${SHORT_HOSTNAME}}"
            csrc="$(readlink -f "$src")"
            if [ "$csrc" != "$src" ]; then
                rm -vf "$dst"
                ln -vs "$src" "$dst"
            fi
        fi
    done
    unset src
    unset csrc
    unset dst
fi

# these override actual tools, so place them at the very end...
alias ps='myps'
alias which='mywhich'
alias ssh='retitlessh'

ihave discard && alias rm=discard || :
ihave colordiff && alias diff=colordiff || :
