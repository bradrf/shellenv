# Environment
# ###########

# TODO: split this up into more sharable pieces (i.e. stuff for osx, stuff for rails, stuff for git)
# TODO: reload doesn't work when root
# TODO: make bundle function to prompt if there is no rvm gemset established
# TODO: make z work for other users (e.g. after rootme, use same .z entries, or, maybe copy it over?)
# TODO: add helper to locate log file (gzip or not) that should contain a timestamp
#       e.g. look at first log time and mtime of file (i.e. last time written to)

# TODO: add "fast" login that skips .bash init files (i.e. ssh-without env)

# NOTE: use tee to peek into a pipeline: cat foo | grep bar | tee /dev/tty | grep same

. "${HOME}/.bashtools"

[[ -n "$TMPDIR" ]] || export TMPDIR="$(dirname "$(mktemp -u)")/"

export WORKDIR="${HOME}/work"
[[ -d "$WORKDIR" ]] || mkdir -p "$WORKDIR"

if ihave go; then
    # Golang expects all projects in a common "src" location for dependency resolution.
    export GOPATH="${WORKDIR}/go"
    [[ -d "$GOPATH" ]] || mkdir -p "${GOPATH}/src"
fi

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
    "/usr/local/opt/go@1.13/bin" \
    '/usr/local/android-studio/bin' \
    '/usr/local/heroku/bin' \
    '/usr/local/share/npm/bin' \
    "${NPM_PACKAGES}/bin" \
    "${GOPATH}/bin" \
    '/usr/local/opt/mysql'*'/bin' \
    "${HOME}/Library/Python/3"*"/bin" \
    "${HOME}/Library/Python/2"*"/bin" \
    "${HOME}/.gem/ruby/"**"/bin" \
    "${HOME}/bin" \
    "${HOME}/.local/bin" \
    "${HOME}/.android-sdk/platform-tools" \
    "${HOME}/.cargo/bin"
do
    if [ -d "$d" ]; then
        if ! echo "$PATH" | grep -qE ":${d}(:|\$)"; then
            export PATH="${PATH}:$d"
        fi
    fi
done
unset d

GOOGLE_CLOUD_SDK='/usr/local/src/google-cloud-sdk'
if [ -d "$GOOGLE_CLOUD_SDK" ]; then
    . "${GOOGLE_CLOUD_SDK}/path.bash.inc"
else
    unset GOOGLE_CLOUD_SDK
fi

# Track if we are the superuser.
if [ `id -u` -eq 0 ]; then
    IAMROOT=true
    SUDO=
else
    IAMROOT=false
    SUDO=sudo
fi

# Track if we are ourselves (i.e. not root and not switched from another user via sume).
! $IAMROOT && test -z "$SUDO_USER" && IAMME=true || IAMME=false
export IAMROOT
export IAMME

if ! $IAMME; then
    # Try the sudo user's home, but it may not exist (i.e. if a daemon user).
    eval THEIRHOME=~"${USER}"
    if [ ! -d "$THEIRHOME" ]; then
        THEIRHOME="${TMPDIR}${USER}"
        mkdir -p -m 0700 "${THEIRHOME}"
    fi
    HISTFILE="${THEIRHOME}/.bash_history_${SUDO_USER}"
    alias cdt="cd $THEIRHOME"
fi

INTERACTIVE=false
case $- in
    *i*)
        INTERACTIVE=true
        [ -f /etc/bash_completion ] && . /etc/bash_completion
        [ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion
        [ -n "$GOOGLE_CLOUD_SDK" ] && . "${GOOGLE_CLOUD_SDK}/completion.bash.inc"
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

if [ -f "${HOME}/.pythonrc.py" ]; then
    # Point interactive Python prompt to initialize with this content.
    export PYTHONSTARTUP="${HOME}/.pythonrc.py"
fi

[ -d "${HOME}/lib/python" ] && export PYTHONPATH="${HOME}/lib/python"

[ -d "${HOME}/lib/ruby" ] && export RUBYLIB="${HOME}/lib/ruby"

[ -f "${HOME}/creds/creds.sh" ] && . "${HOME}/creds/creds.sh"

# convert ec2 tags to env vars when running on an instance
test -e /etc/ec2_version && ihave ec2tags && eval "$(ec2tags)"

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
        alias efg='climacs fg'
    elif [ -n "${ALTERNATE_EDITOR}" ]; then
        export EDITOR="${ALTERNATE_EDITOR}"
    fi

    SHORT_HOSTNAME=$(hostname -s)
    export SHORT_HOSTNAME

    if ! type -t __git_ps1 >/dev/null 2>&1; then
        # no-op this for our prompt below
        function __git_ps1()
        {
            :
        }
    fi

    function __rcs_ps1()
    {
        local ps="$(__git_ps1 "%s")"
        [ -z "$ps" ] && ps="$(hg_branch)"
        [ -n "$ps" ] && echo " $ps"
    }

    if ! ihave rvm-prompt; then
        function rvm-prompt()
        {
            :
        }
    fi

    # TODO: use a single "more info" group of reporting at the end instead of just interpreter
    #       i.e. an "environmental info group" including ruby/python/git/hg/kubectl/etc info
    function __interpreter_prompt()
    {
        if [[ -n "$VIRTUAL_ENV" ]]; then python --version 2>&1; else rvm-prompt; fi
    }

    # NOTE: to pin kubectl at a particular version, follow these steps:
    #       http://zoltanaltfatter.com/2017/09/07/Install-a-specific-version-of-formula-with-homebrew/
    #       but remember to run `brew pin kubernetes-cli` before updating
    if ihave kubectl; then
        function __kctx_prompt()
        {
            echo " \e[0;35m$(shortyk8s_prompt)"
        }
    else
        alias __kctx_prompt=
    fi

    # Sets up the Bash prompt to better display the current working directory as well as exit status
    # codes from failed commands, and make superuser prompts look distinct.
    if $IAMROOT; then
        DISP_USER="\033[0;1;31m-[ ROOT ]-\033[0;1;35m ${SHORT_HOSTNAME}"
    else
        DISP_USER="${USER}@${SHORT_HOSTNAME}"
    fi

    [ -n "$SSH_CLIENT" ] && mc=36 || mc=32
    # automatically updates command history file after each command (use "uh" alias to update to latest)
    export PROMPT_COMMAND="
LASTEXIT=\$?;
_z --add \"\$(command pwd 2>/dev/null)\" 2>/dev/null;
history -a;
printf '\e[34m%s\e[0m ' \$(smart-stamp $$);
[[ -n \"\$FLASH\" ]] && printf \"\e[1;31m\${FLASH}\e[0m\";
[[ \$LASTEXIT -ne 0 ]] && printf \" \e[1;31m[\${LASTEXIT}]\e[0m\";
printf \"\e[${mc}m\${DISP_USER}\$(__kctx_prompt)\";
printf \" \e[33m\${PWD}\e[0m \e[36m(\$(__interpreter_prompt)\$(__rcs_ps1))\e[0m\n\""
    export PS1='# '
    export PS2=' '

    ihave lpass && export LPASS_AGENT_TIMEOUT=3600
fi

UNAME=`uname`
if [ "$UNAME" = 'Darwin' ]; then
    export DARWIN=true
    export BASH_SILENCE_DEPRECATION_WARNING=1
else
    export DARWIN=false
fi

qmakepath=`\which qmake 2>/dev/null`
[ -n "$qmakepath" ] && export QTDIR="$(dirname "$(dirname "$qmakepath")")"

PERL_BASE="${HOME}/perl5"
[ -d "$PERL_BASE" ] && eval "$(perl -I"${PERL_BASE}/lib/perl5" -Mlocal::lib)"

# FIXME: This is a work-around for Wireshark (invalid unclassed pointer in cast to 'GObject')
#export LIBOVERLAY_SCROLLBAR=0


# Shell Options
# #############

if $INTERACTIVE; then
    # notify immediately of background job state changes
    set -b

    shopt -s cdspell
    shopt -s dirspell 2>/dev/null
    shopt -s autocd 2> /dev/null
    shopt -s checkwinsize

    bind 'set completion-ignore-case on'
    bind 'set completion-map-case on' # hyphens/underscore tabcomplete
    bind 'set show-all-if-ambiguous on'
fi

ulimit -n 1024 # open files (default on osx is 256!)

# this is necessary for called things like ruby to access the var...
export COLUMNS
export LINES

# effectively disable screen locking (i.e. accidental ctrl-o x)
export LOCKPRG='/bin/true'


# History Options
# ###############

HISTCONTROL=ignoredups:erasedups
HISTSIZE=500000
HISTFILESIZE=500000
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear:reset:reload"
shopt -s histappend
# see prompt command for additional history sync magix and the "uh" alias

# Aliases
# #######

alias uh='history -n' # re-read from history file (to update from other sessions)
alias l='ls -hal'
alias ll='ls -al'
alias lr='list_recent -hal'
function lf() { list_recent '' "$1" -1; }
alias llr='list_recent -al'
alias mvf='action_most_recent mv'
alias cpf='action_most_recent cp'
alias rmf='action_most_recent rm'
alias less='less -Rginm'
alias ff='find_file'
alias lesstrunc='less -S'
alias trunc="cut -c -\$COLUMNS"
alias funcs='declare -F | grep -vF "declare -f _"'
alias func='declare -f'
alias ppath="echo \"\$PATH\" | tr ':' '\n'"
alias rcopy='rsync -avzC'
alias reload="exec ${SHELL} --login"
alias reload_clean='exec env -i HOME=$HOME TERM=$TERM USER=$USER bash --login --norc --noprofile'
alias nohist='export HISTFILE=/dev/null'
alias base64creds="ruby -rbase64 -e 'puts Base64.urlsafe_encode64(ARGV[0].strip+\":\"+ARGV[1].strip)'"
alias reniceme='renice 10 $$'
alias rootme='sudo -s'
alias rcopy='rsync -avzC --exclude .hg/ --exclude node_modules/'
alias zipdir='zip -9 -r --exclude=*.svn* --exclude=*.git* --exclude=*.DS_Store* --exclude=*~'
alias rcs='rails c -s'
alias rr='rails r'
alias dmesg='dmesg -T'
alias suniq='awk '\''!x[$0]++'\''' # "stream" uniq (tracks previous matches in memory...)
alias cls='printf "\033c"' # blows away screen instead of "clear" which just adds newlines
alias rmbak="\find . \( -name .svn -o -name .git -o -name .hg \) -prune -o -name '*~' -print0 | xargs -0 rm -vf"
alias notecat='cat - >/dev/null'
alias grepl='grep --line-buffered' # good for piping and still seeing the data
alias lps='pv --line-mode --rate > /dev/null' # good for count lines-per-second from stdin
alias grepc='grep -B5 -A"$(( LINES - 10 ))"'
alias each='xargs -tn1'
alias fastdu='ncdu -rx1' # do not cross file systems; run read-only; don't use curses during scan

ihave pry && alias irb='pry'
ihave docker && alias sd='sudo docker'

if ihave bat; then
    #export LESSOPEN='|bat --color always --decorations always %s'
    alias less=bat
    alias cat='bat --paging=never -p'
fi

# teach ansible to use the same control path as ~/.ssh/config does
export ANSIBLE_FORKS=20
export ANSIBLE_SSH_PIPELINING=True
export ANSIBLE_SSH_ARGS='-C -o ControlMaster=auto -o ControlPersist=1h'
export ANSIBLE_SSH_CONTROL_PATH='~/.ssh/cm/%%C'

# Wrap each argument as a independent grep expression for search through command history.
function ghist()
{
    local t cmd='history'
    for t in "$@"; do
        cmd="${cmd} | grep -e $(shellwords "$t")"
    done
    eval $cmd
}

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
    echo "${runner} $(shellwords "$@")" >&2
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

if [ -d "${WORKDIR}/adt" ]; then
    alias adb="${WORKDIR}/adt/sdk/platform-tools/adb"
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
    alias chrome='open -a /Applications/Google\ Chrome.app'
    alias vlc='open -a /Applications/VLC.app'
    alias javare='/Library/Internet\ Plug-Ins/JavaAppletPlugin.plugin/Contents/Home/bin/java'
    alias eject='hdiutil eject'
    alias rtail='tail -r'
    # alias cal='\cal | grep -wFC6 "$(date +%e)"'
    alias calyear='\cal "$(date +%Y)"'

    ihave flock || alias flock="ruby ${RUBYLIB}/flock.rb"

    f="/Applications/VMware Fusion.app/Contents/Library/vmrun"
    [ -x "$f" ] && alias vmrun="\"$f\""
    unset f
else
    ihave tac && alias rtail='tac'

    if ihave systemctl; then
        alias servicels='systemctl -l --type service --all --plain | awk "/^  /{print \$1}" | sort | uniq'
    elif ihave service && ihave initctl; then
        alias servicels='(service --status-all 2>&1 | rev; initctl list) | cut -d" " -f1 | sort | uniq'
    elif ihave service; then
        alias servicels='service --status-all 2>&1 | rev | cut -d" " -f1 | sort | uniq'
    elif ihave initctl; then
        alias servicels='initctl list | cut -d" " -f1 | sort | uniq'
    fi

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
                $SUDO rmmod uas usb_storage nls_utf8 hfsplus
        }
    fi
fi

if ihave clipi; then
    alias eclipo='eval $(clipo)'
    alias markdownitc='clipo|fold -s -w 100|markdownit code|clipi'
    alias markdownitb='clipo|fold -s -w 100|markdownit block|clipi'
    alias clipd='date|clipi'
fi


# Functions
# #########

function simplify_prompt()
{
    unset PROMPT_COMMAND
    export PS1='> '
}

# run with only system configuration files
function norc_prompt()
{
    env -i HOME="$HOME" bash --init-file /etc/profile
}

function psgrep()
{
    local wide=false
    if [[ "$1" = '-w' ]]; then
        wide=true; shift
    fi
    local srch="$*"
    local gargs
    hasupper "$srch" || gargs='-i'
    \ps auxwwww | grep $gargs -E '^USER|'"[${srch:0:1}]${srch:1}" | \
        (if $wide; then cat; else cut -c -$COLUMNS; fi)
}

if ! ihave tree; then
    function tree()
    {
        ls -R "$@" | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'
    }
fi

function list_recent()
{
    local ls_args=$1
    local dir=${2:-.}
    local head_args=${3}
    dir=${dir%/}
    ls -td $ls_args {"$dir"/.*,"$dir"/*} | grep -Ev '^total |\.\.?$' | head $head_args
}

function action_most_recent()
{
    local cmd=$1; shift
    local dir=$1; shift
    $cmd "$(lf "$dir")" "$@"
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
        local args=(-i) m='.'

        while getopts 'palntu' opt; do
            case $opt in
                p) ;;
                a) sudo=$SUDO;;
                l) m='-F LISTEN';;
                n) args=("${args[@]}" -nP);;
                t) args=${args[@]/-i/-iTCP};;
                u) args=${args[@]/-i/-iUDP};;
                ?)
                    echo 'usage: osxnetstat [pantu]' >&2
                    return 1
            esac
        done

        $sudo lsof ${args[@]} | grep $m
    }

    function dman()
    {
        open "dash://man:$@"
    }

    function dash()
    {
        open "dash://$@"
    }

    if ihave terminal-notifier; then
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
    fi

    # NOTE: the disable of the firewall isn't complete, for some reason still bitches about allowing
    # incoming connections until i use the UI... must need some other toggle...
    # "accept incoming network connections" popup
    # may need to restart the firewall?
    function allow_all()
    {
        local restart=false

        case $1 in
            status)
                echo "Gatekeeper has $(spctl --status)"
                local v=$(defaults read /Library/Preferences/com.apple.alf globalstate)
                case "$v" in
                    0) echo 'Firewall is DISABLED';;
                    1) echo 'Firewall is enabled for specific apps/services (normal)';;
                    2) echo 'Firewall is enabled for essential services (very strict)';;
                    *) echo "Firewall state is UNKNOWN: $v"; return 2;;
                esac
                ;;
            open)
                echo '*** ALLOWING all apps and DISABLING firewall'
                $SUDO spctl --master-disable && \
                    $SUDO defaults write /Library/Preferences/com.apple.alf globalstate -int 0 && \
                    restart=true
                ;;
            protect)
                echo '*** Enabling gatekeeper and firewall'
                $SUDO spctl --master-enable && \
                    $SUDO defaults write /Library/Preferences/com.apple.alf globalstate -int 1 && \
                    restart=true
                ;;
            *)
                echo 'allow_all { status | open | protect }' >&2
                return 1
        esac

        if $restart; then
            $SUDO launchctl unload /System/Library/LaunchDaemons/com.apple.alf.agent.plist && \
                $SUDO launchctl load /System/Library/LaunchDaemons/com.apple.alf.agent.plist
        fi
    }

    function find_all_app()
    {
        local xopts=(-0)
        if [[ "$1" = '--purge' ]]; then
            shift; xopts+=(rm -rf)
        else
            xopts+=(-n 1 echo)
        fi

        local namearg
        if [[ "$1"  = '-s' ]]; then
            shift; namearg='-path'
        else
            namearg='-ipath'
        fi

        if [[ $# -ne 1 ]]; then
            echo 'usage: find_all_app [--purge] [-s] <appname>' >&2
            return 1
        fi

        local sn skipdirs=()
        for sn in chrome safari tunnelblick CommandLineTools; do
            if [[ "$@" != "${sn}" ]]; then
                [[ ${#skipdirs} -gt 0 ]] && skipdirs+=(-o)
                skipdirs+=(-ipath "*${sn}*")
            fi
        done

        (
            find /Applications -maxdepth 1 "${namearg}" "*${@}*" -print0 && \
            find ~/Library \( "${skipdirs[@]}" \) -prune -o "${namearg}" "*$@*" -print0 && \
            sudo find /Library \( "${skipdirs[@]}" \) -prune -o "${namearg}" "*$@*" -print0
        ) | sudo xargs "${xopts[@]}"
    }

fi # DARWIN

function resize_terminal()
{
    if [ $# -ne 2 ]; then
        echo 'usage: resize <height> <width>' >&2
        return 1
    fi
    printf '\e[8;'$1';'$2't'
}

# Changes the terminal's and screen's titles to whatever text passed in (or to the previously set
# title if no arguments are provided). Exported to allow use by scripts like rshick.
export RETITLE_CURRENT=sh
export RETITLE_PREVIOUS="$RETITLE_CURRENT"
function retitle()
{
    [ -n "$SSH_CLIENT" ] && return 0
    if [ $# -lt 1 ]; then
        RETITLE_CURRENT="$RETITLE_PREVIOUS"
    elif [ "$RETITLE_CURRENT" != "$*" ]; then
        RETITLE_PREVIOUS="$RETITLE_CURRENT"
        RETITLE_CURRENT="$*"
    fi
    # conditionally sets both a terminal title and a screen title
    [[ "$TERM" = 'screen'* ]] && echo -n -e "\\033k${RETITLE_CURRENT}\\033\\"
    printf "\\033]0;${RETITLE_CURRENT}\\007"
}
export -f retitle

if ihave ssh-config; then
    # run a command asynchronously for all matching ssh hosts
    function ssh_each()
    {
        # echo "for h in `ssh-config list | awk '/volume[234]0-/{print $2}'`; do echo $h; ssh -o 'StrictHostKeyChecking=no' $h -- 'sudo apt -qy purge mlocate' & done; wait" | clipi
        if [[ $# -lt 2 ]]; then
            :
        fi

        local host
    }
fi

# kill off all the background ssh masters
function ssh_clean()
{
    if [[ $# -eq 0 ]]; then
        pkill -f 'ssh.*/.ssh/cm'
    else
        pkill -f 'ssh.*/.ssh/cm.*'"$1"
    fi
}

function reload_ssh_config()
{
    local scfn="${HOME}/.ssh/config"
    \rm -f "$scfn"
    shopt -s nullglob # empty list if no match
    cfns=("${scfn}_"*)
    cfns=(`echo ${cfns[@]//*~/}`) # ignore emacs backups
    (
        cat <<EOF
# **********************************************************************
# Generated from: ${scfn}base ${cfns[@]}
# **********************************************************************

EOF
        awk '/^# DEFAULTS ##*$/{exit}1' "${scfn}base"
        [ ${#cfns[@]} -gt 0 ] && cat "${cfns[@]}" && echo
        awk '/^# DEFAULTS/,0' "${scfn}base"
        echo
    ) > "$scfn"
    unset cfns
    shopt -u nullglob
    ihave aws_ssh_config && aws_ssh_config
    ihave gssh-save-config && gssh-save-config
    chmod 400 "$scfn"
}

# call retitle with ssh info and reset back to original on exit
# FIXME: this fucks up use of ssh commands (e.g. FOO=`ssh remote-host hostname` will embed escape codes in var!)
function retitlessh()
{
    local name pname rc
    # try picking out the short hostname after skipping options
    for name in "$@"; do
        if [[ "${name}" != '-'* ]]; then
            # only break if previous option was _NOT_ expecting an argument to follow
            if [[ "${pname}" != '-i' ]]; then
                break
            fi
        fi
        pname=$name
    done
    name="${name#*@}"
    if [[ ! "${name}" =~ ^[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}$ ]]; then
        # not an IP address
        name="${name%%.*}"
    fi
    [ -n "$name" ] && retitle "$name"
    \ssh "$@"
    rc=$?
    [ -n "$name" ] && retitle
    return $rc
}
export -f retitlessh

# copy files to or from a remote system that requires different privileges
function sshtar()
{
    local ldir rhost rdir rbase rdirp
    if [ "$1" = '-u' ]; then
        shift; ruser="$1"; shift
    else
        ruser='root'
    fi
    if [ $# -ne 2 ]; then
        cat <<EOF >&2
usage: sshtar [-u <remote_user>] <remote_host> <remote_directory>

  NOTE: The output of this call is tar and is expected to be piped or captured, or
        the input of this call is tar and is expected to be piped on the remote host.

  Examples:
       sshtar foo-host /bar/baz/foozy > foozy.tgz
       sshtar foo-host /var/baz/foozy | tar zx
       tar cz foozy | sshtar foo-host /var/baz
EOF
        return 1
    fi
    rhost="$1"; shift
    rdir="$1"; shift
    if istty in; then
        rbase="$(basename "$rdir")"
        rdirp="$(dirname "$rdir")"
        \ssh "$rhost" "sudo -u ${ruser} tar -C '${rdirp}' -cz '${rbase}'"
    else
        \ssh "$rhost" "sudo -u ${ruser} tar -C '${rdir}' -xz"
    fi
}

if $DARWIN; then
    function flush_dns_cache()
    {
        $SUDO killall -HUP mDNSResponder
        ihave discoveryutil && $SUDO discoveryutil mdnsflushcache
        ihave dscacheutil && $SUDO dscacheutil -flushcache
    }
else
    function flush_dns_cache()
    {
        echo 'not implemented' >&2
        return 1
    }
fi

function host_is_ipv4()
{
    grep -qE '^ *\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3} *$' <<< "$1"
}

function host_to_addrs()
{
    if [ $# -lt 1 ]; then
        echo 'host_to_addrs <hostname> [<hostname>...]' >&2
        return 1
    fi
    local hn
    for hn in "$@"; do
        if host_is_ipv4 "$hn"; then
            echo "$hn"
        else
            nslookup "$hn" | awk '/Address/{if ($2 !~ "#") print $2}'
        fi
    done
}

# add IPs for host into /etc/hosts
function host_alias()
{
    if [[ $# -ne 2 ]]; then
        echo 'host_alias <src_hostname> <dst_hostname>' >&2
        return 1
    fi
    host_unalias "$2" >/dev/null
    local addr
    for addr in $(host_to_addrs "$1"); do echo "${addr} ${2}"; done | $SUDO tee -a /etc/hosts
    flush_dns_cache
    cat /etc/hosts
}

# remove entries added by host_alias
function host_unalias()
{
    if [[ $# -ne 1 ]] || [[ -z "$1" ]]; then
        echo 'host_unalias <alias_hostname>' >&2
        return 1
    fi
    $SUDO sed -i '' '/^[^#]* '"$1"'/d' /etc/hosts # remove existing entries (not commented)
    cat /etc/hosts
}

if ihave mtr; then
    alias mtr_report='mtr --report-wide --show-ips --tcp --port 443 -c 20'

    # runs async mtr reports for all addresses found in a hostname's record
    function mtr_all()
    {
        if [ $# -lt 1 ]; then
            echo 'mtrall <hostname> [<hostname>...]' >&2
            return 1
        fi
        [ -n "$SUDO" ] && $SUDO -v
        (
            local rptdir="$(mktemp -d "${TMPDIR}mtrallXXX")"
            cd "$rptdir"
            local addr
            for addr in $(host_to_addrs "$@"); do
                $SUDO mtr --report-wide --show-ips --tcp --port 443 -c 20 "$addr" > "${addr}.rpt" &
            done
            wait
            cat *.rpt
            rm -rf "$rptdir"
        )
    }
fi

# dump remote network traffic
# NOTE: the output of this call is pcap and is expected to be piped or captured; examples:
#       sshdump foo-host tcp port 80 > tcp80.pcap
#       sshdump foo-host -i eth0 tcp port 80 | tshark -i -
# function sshdump()
# {
#     local rhost
#     if [ $# -lt 1 ]; then
#         echo 'usage: sshdump <remote_host> [<tcpdump_options>...]' >&2
#         return 1
#     fi
#     rhost="$1"; shift
#     # packet buffered, no dns, and full snaplen written to stdout
#     \ssh "$rhost" sudo tcpdump -Uns0 -w - "$(shellwords "$@")"
# }
# complete -F _ssh sshdump
#
# wireshark now provides this!!! man sshdump

alias dumpifs='tcpdump -D' # list network interfaces available to tcpdump
alias httpdump='asciidump'
function asciidump()
{
    if [ $# -lt 1 ]; then
        cat <<EOF >&2
usage: asciidump <interface> [<pcap_filter>...]
dump TCP PUSH frames as ASCII (or all UDP if used in <pcap_filter>)
$(list_iface_ips inet)
EOF
        return 1
    fi
    local iface="$1"; shift
    local filter
    if [ $# -gt 0 ]; then
        if [[ "$*" == *udp* ]]; then
            filter="$@"
        else
            filter='tcp[tcpflags] & tcp-push != 0'" and $@"
        fi
    fi
    $SUDO tcpdump -AKlqns0 -i "$iface" $filter
}

# TODO: implement rolling dump to run tcpdump and periodically launch new capture and terminate current
#       SEE -C and -G options! it has this built-in!
#       ALSO -z (postrotate command! for zipping)
function rollingdump()
{
    :
}

# TODO: consider adding a rolling file capture (i.e. reads stdin and writes to rolling output w/ zip)
#       for sshdump ..... > rolling-output -z mysave-file-XXX.pcap.gz

DOWNLOADS_DIR="${HOME}/Downloads"
[ -d "$DOWNLOADS_DIR" ] || mkdir -p "$DOWNLOADS_DIR"

function download()
{
    local opts bn hfn dst tfn rc hrc

    if [ "$1" = '-v' ]; then
        opts='-v'; shift
    fi

    if [ $# -ne 1 -a $# -ne 2 ]; then
        echo 'usage: download [-v] <url> [<destination>]' >&2
        return 1
    fi

    if ! ihave curl; then
        echo 'todo: need to add support for wget' >&2
        return 2
    fi

    bn="$(basename "$1")"
    dst="${2:-${DOWNLOADS_DIR}}"

    if [ -f "$dst" ]; then
        :                       # overwrite existing file
    elif [ -d "$dst" ]; then
        dst="${dst}/${bn}"      # drop into a directory
    elif [[ "$dst" != */* ]]; then
        dst="${DOWNLOADS_DIR}/${dst}" # user filename
    else
        :                       # user directory/filename
    fi

    hfn="${dst}.headers"
    tfn="$(mktemp)"

    # use etags to only download if changed from server
    # move temp files only if successful (checking both curl success and return code in headers)

    curl $opts --dump-header "${tfn}.headers" -sSL \
        -H "If-None-Match: $(sed 's/ETag: \(.*\)/\1/p;d' "$hfn" 2>/dev/null)" \
        -o "${tfn}" \
        -w "Response: %{http_code}\nDownloaded: %{size_download} bytes\n" "$1" \
        && \
        awk 'NR==1{exit($2>299)}' "${tfn}.headers" && \
        mv -f "${tfn}" "${dst}" && mv -f "${tfn}.headers" "${hfn}" && \
        echo "Saved: ${dst}"
    rc=$?
    [ $rc -ne 0 ] && \rm -f "${tfn}"*
    return $rc
}

# figure out which download tool to use
if ihave curl; then
    httpget='curl -kqfs'
else
    httpget='wget -q --no-check-certificate -O -'
fi

function list_iface_ips()
{
    if [ $# -ne 1 ]; then
        echo 'usage: list_iface_ips { inet | inet6 }' >&2
        return 1
    fi
    ifconfig | awk '
/^[a-z0-9]+/ {
    sub(/:.*$/,"",$1); iface=$1
}
/'"$1"' / {
    a=($2 == "addr:" ? $3 : $2)
    sub(/^addr:/,"",a)
    print iface, a
}'
}

function modify_files()
{
    if [[ $# -ne 1 ]]; then
        echo 'usage: modify_files <directory>' >&2
        return 1
    fi

    local fn cnt=0
    for fn in "$1"/*; do
        printf '\r%s' "$fn"
        # at offset 129, write 128 random bytes (to avoid stomping on headers)
        dd conv=notrunc if=/dev/urandom of="$fn" bs=1 skip=128 count=128 >/dev/null 2>&1 || return
        (( ++cnt ))
    done
    printf '\rmodified %d files in %s\n' $cnt "$1"
}

function generate_files()
{
    local start=1
    if [[ "$1" = '--start' ]]; then
        shift; start=$1; shift
    fi

    if [[ $# -ne 4 ]]; then
        cat <<EOF >&2
usage: generate_files [--start <num>] { png | jpg | gif | svg | txt | * } <count> <size> <directory>

       Rough size speed estimates (per second):
         * Images:  2000
         * Binary:  200000
         * Text:    5000000

       Image sizes are width unless specified as WIDTHxHEIGHT (i.e. height defaults to 1).

       PNG results in larger files for the same amount of time to generate (GIF is smallest).

       Any extension not show above will have binary content.
EOF
        return 1
    fi

    local ext=$1; shift
    local count=$1; shift
    local size=$1; shift
    local dir=$1; shift

    mkdir -p "$dir"

    local i=0 num fn
    while [[ $i -lt $count ]]; do
        num=$(( start + i++ ))
        fn="${dir}/file$(printf %03d $num).${ext}"
        printf '\r%s' "$fn"

        case "$ext" in
            svg)
                if ! ihave potrace; then
                    echo 'Generating SVG requires "potrace" utility to be installed' >&2
                    return 2;
                fi
                convert -size "$size" plasma:fractal "$fn" || return
                ;;
            png|jpg|gif)
                convert -size "$size" plasma:fractal "$fn" || return
                ;;
            txt)
                cat /dev/urandom | LC_CTYPE=C tr -dc '[:print:]' | \
                    fold -w 80 | head -c "$size" > "$fn" || return
                ;;
            *)
                dd if=/dev/urandom of="$fn" bs=1 count="$size" >/dev/null 2>&1 || return
                ;;
        esac
    done
    echo
}

function get_iface_ip()
{
    if [ $# -ne 2 ]; then
        echo 'usage: get_iface_ips { inet | inet6 } <interface_name>' >&2
        return 1
    fi
    list_iface_ips "$1" | awk '/^'"${2}"'/{print $2}'
}

function getmyip()
{
    local hn='api.ipify.org'
    case "$1" in
        ssl)
            echo "${httpget} https://$hn"
            echo "$($httpget https://$hn)"
            ;;
        lo*)
            echo 'ifconfig'
            list_iface_ips inet | awk '{ if (!match($2,/127\.0\.0\./)) print $2 }'
            ;;
        *)
            echo "${httpget} http://$hn"
            echo "$($httpget http://$hn)"
    esac
}

# https://superuser.com/questions/565991/how-to-determine-the-socket-connection-up-time-on-linux
function get_socket_uptime()
{
    local addr=${1:?Specify the remote IPv4 address}
    local port=${2:?Specify the remote port number}
    # convert the provided address to hex format
    local hex_addr=$(python -c "import socket, struct; print(hex(struct.unpack('<L', socket.inet_aton('$addr'))[0])[2:10].upper().zfill(8))")
    local hex_port=$(python -c "print(hex($port)[2:].upper().zfill(4))")
    # get the PID of the owner process
    local pid=$(netstat -ntp 2>/dev/null | awk '$6 == "ESTABLISHED" && $5 == "'$addr:$port'"{sub("/.*", "", $7); print $7}')
    [ -z "$pid" ] && { echo 'Address does not match' 2>&1; return 1; }
    # get the inode of the socket
    local inode=$(awk '$4 == "01" && $3 == "'$hex_addr:$hex_port'" {print $10}' /proc/net/tcp)
    [ -z "$inode" ] && { echo 'Cannot lookup the socket' 2>&1; return 1; }
    # query the inode status change time
    local timestamp=$(find /proc/$pid/fd -lname "socket:\[$inode\]" -printf %T@)
    [ -z "$timestamp" ] && { echo 'Cannot fetch the timestamp' 2>&1; return 1; }
    # compute the time difference
    LANG=C printf '%s (%.2fs ago)\n' "$(date -d @$timestamp)" $(bc <<<"$(date +%s.%N) - $timestamp")
}

# show info for all certs in a chain pem
function getcertinfo()
{
    local c
    if [[ "$1" = '--all' ]]; then
        c='1'; shift
    else
        c='/^Certificate|Issuer:|Subject:|DNS:|Before|After/{print}'
    fi
    openssl crl2pkcs7 -nocrl -certfile "$1" | openssl pkcs7 -print_certs -noout -text | awk "$c"
}

# get info for all certs presented by host
function getservercerts()
{
    local h=$(awk -F[/:] '{if ($4){print $4}else{print}}' <<< "$1")
    local fn="${h}-chain.pem"
    [[ "$h" =~ ':' ]] || h+=':443'
    echo | openssl s_client -connect "$h" 2>/dev/null -showcerts "${args[@]}" | \
        awk 'BEGIN{f=0} /BEGIN/{f=1} f{print} /END/{f=0}' >"$fn"
    getcertinfo "$fn"
}

function colprint()
{
    awk '{print $'"$1"'}'
}

function colcmp()
{
    if [ $# -lt 1 ]; then
        echo 'usage: colcmp [-v] <expression> [<files...>]' >&2
        return 1
    fi
    awk '{if('"$1"')print}'
}

function colgrep()
{
    local match
    if [ "$1" = '-v' ]; then
        shift; match='!~'
    else
        match='~'
    fi
    if [ $# -lt 2 ]; then
        echo 'usage: colgrep [-v] <column_number> <egrep_expression> [<files...>]' >&2
        return 1
    fi
    local col="$1"; shift
    local exp="$1"; shift
    awk '$'"$col"' '"$match"' /'"$exp"'/{print}' "$@"
}

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

function find_file()
{
    if [[ $# -lt 1 ]]; then
        echo 'usage: find_file <dir> [<dir> ...] <file_pattern> [<file_pattern> ...] [<find_args...>]' >&2
        return 1
    fi

    local dirs=()
    local fargs=()
    while [[ -d "$1" ]]; do dirs+=("$1"); shift; done
    while [[ $# -gt 0 && "$1" != -* ]]; do fargs+=(-iname '*'"$1"'*'); shift; done

    local args=("${dirs[@]}" \( -name .svn -o -name .git -o -name .hg \) -prune -o \
                             -not -name '*~' -type f "${fargs[@]}" -print "$@")
    echo "find$(printf ' %q' "${args[@]}")" >&2
    find "${args[@]}"
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
    appscfg="${HOME}/.gitconfig_apps"
    [ -f "$appscfg" ] || touch "$appscfg"
    if ihave diff-so-fancy; then
        git config -f "$appscfg" --replace pager.diff 'diff-so-fancy | less --tabs=1,5 -RFX'
        git config -f "$appscfg" --replace pager.show 'diff-so-fancy | less --tabs=1,5 -RFX'
    fi
    unset appscfg

    function gitopen()
    {
        local path="${1-.}"
        local url
        url=$(git -C "${path}" remote -v | \
                  awk '/fetch/{sub(/git@|git:\/\//,"",$2);sub(/:/,"/",$2);print "https://"$2}')
        [[ -n "$url" ]] || return
        local bn
        local rurl
        rurl="$(curl -o /dev/null -qfsw '%{redirect_url}' "$url")"
        [[ -n "$rurl" ]] && url=$rurl
        bn="$(git -C "$path" symbolic-ref HEAD 2>/dev/null)"
        bn=${bn##refs/heads/}
        [[ -n "$bn" ]] && url+="/tree/$bn"
        open "$url"
    }

    function gitsetbranchname()
    {
        local path="${1-.}"
        branch_name="$(git -C "$path" symbolic-ref HEAD 2>/dev/null)" ||
            branch_name="(unnamed branch)" # detached HEAD
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

    function gitlogfilecounts() {
        git log --stat --name-status "$@" | ruby -e \
'def dump
  $s and puts $s.map{|k,v|k+"="+v.to_s}
  $s=Hash.new(0)
end
sp=false
while l = gets
  if l.strip == ""
    sp or puts
    sp = true
    next
  end
  sp = false
  l =~ /^commit/ and dump
  l =~ /^(.)\t/ ? $s[$1] += 1 : puts $_
end
dump'
    }

    function gitparse() {
        echo "$*" | sed -n 's/^ *\([^@]*\)@\([^:]*\):\([^\/]*\)\/\(.*\).git *$/urluser="\1";host="\2";gituser="\3";path="\4"/p'
    }
fi

function pdfcat()
{
    if [ $# -ne 1 ]; then
        cat 'usage: pdfcat <pdf_filename>' >&2
        return 1
    fi
    # no page breaks and preserve layout where possible
    pdftotext -nopgbrk -layout "$1" -
}

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

# FIXME (happens when exiting pry as nobody):
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

function sush()
{
    local uid uidname
    idas "$1" && shift

    if [ -z "$uid" ]; then
        echo 'usage: sush [<user>] <cmd...> ' >&2
        return 1
    fi

    sudo -u "$uidname" sh -c "$*"
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
    if ihave lnav; then
        PT_PAGER=lnav
    else
        PT_COLOR='--force-color'
        PT_PAGER=less
    fi
    function pt()
    {
        if [[ "$*" = *"-h"* ]]; then
            papertrail "$@"
            return
        fi
        if test -n "$PT_PAGER" && istty out; then
            papertrail $PT_COLOR "$@" | $PT_PAGER
        else
            papertrail "$@"
        fi
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
        local pid
        for pid in `pgrep "$@"`; do
            cat <<EOF
-- ${pid} --------------------------------------------------------------------
EOF
            $SUDO cat "/proc/${pid}/environ" | tr '\0' '\n' | sort
        done
    }
elif $DARWIN; then
    function penv()
    {
        $SUDO \ps -wwwwE $1
    }
fi

# Bash supports TCP connections: http://tldp.org/LDP/abs/html/devref1.html
function find_open_port()
{
    if [ $# -ne 1 ]; then
        echo 'usage: find_open_port <starting_port>' >&2
        return 1
    fi

    local port=$1
    while [ $port -lt 65536 ]; do
        # use a subshell to ensure file handles are closed on exit
        if ! (exec 6<>/dev/tcp/127.0.0.1/$port) 2>/dev/null; then
            # connection failed, good to go!
            echo $port
            return 0
        fi
        (( port++ ))
    done
    return 2
}

# List TCP network connections for process(es).
function pnet()
{
    local pids
    pids="$(join , $(pgrep "$@"))"
    $SUDO lsof -a -nP -iTCP -p $pids
}

if ! $DARWIN; then
    function ptop()
    {
        local targs='-c'
        if [ "$1" = '-targs' ]; then
            shift; targs="${targs} $1"; shift
        fi
        top $targs -p "$(join , $(pgrep "$@"))"
    }
fi

if ihave htop; then
    # FIXME: DRY up ptop/phtop, same logic!
    function phtop()
    {
        local targs=''
        if [ "$1" = '-targs' ]; then
            shift; targs="${targs} $1"; shift
        fi
        htop $targs -p "$(join , $(pgrep "$@"))"
    }
fi

function forever()
{
    until "$@"; do
        echo "'$*' crashed with exit code $?.  Respawning..." >&2
        sleep 1
    done
}

# Wait for processes to exit
function pwait()
{
    local watch=false
    if [[ "$1" = '--watch' ]]; then
        watch=true; shift
    fi
    local last_cnt=-1 cnt
    while true; do
        cnt=$(pgrep -c "$@")
        if $watch && [[ $last_cnt -ne $cnt ]]; then
            echo "$c"
        fi
        [ $cnt -gt 0 ] || break
        sleep 0.3
    done
}

function wait_tcp()
{
    if [[ $# -ne 3 ]]; then
        echo 'usage: wait_tcp <host> <port> <max_seconds>' >&2
        return 1
    fi

    local host=$1 port=$2 max_seconds=$3
    while ! nc -z $1 $2 >/dev/null 2>&1 && [[ $max_seconds -gt 0 ]]; do
        sleep 1
        (( --max_seconds ))
    done
}

GREEN=$(echo -e '\033[32m')           # green fg
GOOD=$(echo -e '\033[01;32m')         # bold green fg
WARN=$(echo -e '\033[01;33m')         # bold yellow fg
BAD=$(echo -e '\033[01;31m')          # bold red fg
HILIGHT=$(echo -e '\033[30m\033[43m') # black fg, yellow bg
NORMAL=$(echo -e '\033[0m')           # normal

# Highlight any matched line from standard input (STDIN).
function highlight()
{
    if [[ $# -gt 0 ]]; then
        awk '{if (tolower($0) ~ /'"${1}"'/) {print "'"${HILIGHT}"'" $0 "'"${NORMAL}"'"} else {print}}'
    else
        awk '{print "'"${HILIGHT}"'" $0 "'"${NORMAL}"'"}'
    fi
}

# Colorize good, warn, or bad matches from stdin
function match()
{
    if [[ $# -lt 2 ]]; then
        echo 'usage: match [-g <good>] [-w <warn>] [-b <bad>]' >&2
        return 1
    fi
    local args=(-E)
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -g) args+=(-e 's/('"${2}"')/'"${GOOD}"'\1'"${NORMAL}"'/g'); shift; shift;;
            -w) args+=(-e 's/('"${2}"')/'"${WARN}"'\1'"${NORMAL}"'/g'); shift; shift;;
            -b) args+=(-e 's/('"${2}"')/'"${BAD}"'\1'"${NORMAL}"'/g'); shift; shift;;
            *)
                echo "unknown remaining options: $*" >&2
                return 2
        esac
    done
    sed -E "${args[@]}"
}


# Tail a file with a regular expression that highlights any matches from the tail output.
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

    tail $targs | highlight "$1"
}

# Tail a file in the background while another process runs in the foreground, killing off the tail
# when the foreground process is done.
function tailrun()
{
    local logproc tpid cmdrc

    if [ "$1" = '-p' ]; then
        shift
        logproc="$1"
        shift
    else
        logproc='cat'
    fi

    if [ $# -lt 2 -o ! -f "$1" ]; then
        echo 'usage: tailrun [-p <log_processing_block>] <file> <cmd> [<options>...]' >&2
        return 1
    fi

    tpid="$(mktemp "${TMPDIR}/tpid.XXX")"
    ( tail -Fn0 "$1" & echo $! >&3 ) 3>"$tpid" | $logproc &
    shift

    "$@"
    cmdrc=$?
    kill "$(cat "$tpid")"
    rm -f "$tpid"

    return $cmdrc
}

# Tail a file truncating long lines to the width of the terminal.
# Passes all commands to tail. Alows for piping in to less (or see lesstrunc alias above).
function tailtrunc()
{
    tail "$@" | trunc
}

# If the last argument looks like running a specific test by line number, tail the test log,
# otherwise, run it like normal
function rsp()
{
    local force quiet
    if [[ "$1" = '-f' ]]; then force=true; shift; else force=false; fi
    localsetrunner rspec
    \rm -f log/test*
    if $force || [[ "${@: -1}" =~ :[0-9]+$ ]]; then
        touch log/test.log
        tailrun log/test.log "$runner" "$@"
    else
        $runner --order defined "$@"
    fi
}

function rsp_failures()
{
    strip-colors tmp/failing_specs.log | awk '/^rspec /{print}'
}

function rsp_retry()
{
    rsp $(rsp_failures | awk '{print $2}')
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

if ihave gem; then
    function gem_uninstall_all()
    {
        local i
        for i in `gem list --no-versions`; do
            gem uninstall -aIx $i
        done
    }

    function gem_list_installed()
    {
        gem list -ld | awk '/^[^ ]/{print};/Installed at/,/^ *$/{if (!match($0,/^ *$/))print}'
    }
fi

if ihave pip2 && ! ihave pip; then
    ln -s "$(which pip2)" "${HOME}/bin/pip"
fi

# Install notes for OS X (no brew, yet!):
#  * get latest pip using system python: sudo easy_install pip
#  * install latest 3.x: brew install python
#  * see also: https://docs.brew.sh/Homebrew-and-Python
# if ihave pip; then
#     function pip_list()
#     {
#         echo 'System Packages:'
#         echo '--------------'
#         ( pip list --format freeze ; pip list --format freeze --user ) | sort | uniq -u
#         echo
#         echo 'User Packages:'
#         echo '--------------'
#         pip list --format freeze --user
#     }

#     function pip_upgrade()
#     {
#         while read -r; do
#             echo "$REPLY"
#             pip install --user -U "$REPLY"
#         done < <(pip list --format freeze --user --outdated | sed 's/==.*$//')
#     }

#     function pip_reinstall()
#     {
#         while read -r; do
#             echo "$REPLY"
#             pip install --force-reinstall --user -U "$REPLY"
#         done < <(pip list --format freeze --user | sed 's/==.*$//')
#     }

#     function pip_install()
#     {
#         if [ -n "$VIRTUAL_ENV" ]; then
#             pip install "$@"
#             return
#         fi
#         local args sudo
#         if [ "$1" = '--root' ]; then
#             shift
#             sudo='sudo -H'
#         else
#             args='--user'
#         fi
#         local pn="$1"
#         if [[ "$pn" = http:* ]] || [[ "$pn" = https:* ]]; then
#             pn="git+${pn}.git"
#         fi
#         $sudo pip install $args "$pn"
#     }

#     function pip_uninstall()
#     {
#         if [ -n "$VIRTUAL_ENV" ]; then
#             pip uninstall "$@"
#             return
#         fi
#         local args sudo
#         if [ "$1" = '--root' ]; then
#             shift
#             sudo='sudo -H'
#         fi
#         local pn="$1"
#         if [[ "$pn" = http:* ]] || [[ "$pn" = https:* ]]; then
#             pn="git+${pn}.git"
#         fi
#         $sudo pip uninstall $args "$pn"
#     }
# fi

PRETTYCMD='python -mjson.tool'
if ihave pygmentize; then
    PRETTYCMD+='| pygmentize -l json'
    alias prettyjsonlogs='pygmentize -s -l json -O style=monokai'
fi
alias prettyjson=$PRETTYCMD

function tohtml()
{
    if [ $# -ne 1 ]; then
        echo 'usage: tohtml <title>' >&2
        return 1
    fi
    tee >(aha -t "$1" > "$1.html")
}

# converts 1024-based MB/s to 1000-based Mbits/s
function toMbps()
{
    # bytes to megabits: 8 * 1,024 * 1,024 / 1,000,000
    calc "$1 * 8.388608"
}

# converts 1000-based Mbits/s to 1024-based MB/s
function toMBps()
{
    # bits to megabytes: 1,000,000 / (8 * 1,024 * 1,024)
    calc "$1 * 0.11920928955"
}

# converts seconds into days:hours:minutes:seconds
function to_time()
{
    ruby -e "
t = ${1}
mm, ss = t.divmod(60)
hh, mm = mm.divmod(60)
dd, hh = hh.divmod(24)
puts [dd, hh, mm, ss].map{|i|'%02d'%i}.join(':')
"
}

# converts nearly any time/date into local time/date as iso-8601
# (even crazy unity editor.log times)
function time_parse()
{
    local cmd fcmd fmt=$ISO8601_FMT

    while true; do
        case "$1" in
            -f) shift; fmt=$1; shift;;
            -u) shift; fcmd+='.utc';;
            *)  break;
        esac
    done

    if [[ $# -lt 1 ]]; then
        echo 'usage: time_parse [-u] [-f <time_format>] <string>...' >&2
        return 1
    fi

    local str="$*" istr

    if [[ $# -eq 1 && "${str}" =~ ^\ *[\.0-9]+\ *$ ]]; then
        istr=${str%.*} # remove everything after a decimal
        if [[ ${str%.*} -gt 0xffffffff ]]; then
            # microseconds
            cmd="at(${str} / 1000000.0)"
        else
            cmd="at(${str})"
        fi
    elif [[ "${str}" =~ ^\ *[0-9]{4}-[0-9]{4}\ [0-9]{2}:[0-9]{2}:[0-9]{2} ]]; then
        cmd="strptime('${str}'.strip, '%Y-%m%d %T %Z')"
    else
        cmd="parse('${str}')"
    fi

    ruby -rtime -e "puts Time.${cmd}.localtime${fcmd}.strftime('${fmt#+}')"
}

# converts 1024-based MB of data and 1000-based Mbits/s rate into hours:minutes:seconds
function file_tx_calc()
{
    if [ $# -ne 3 ]; then
        echo 'usage: file_tx_calc <MB_of_data> <Mbps_rate> <overhead_percent>' >&2
        return 1
    fi
    local mbit=`toMbps $1`
    local seconds=`calc "${mbit} / $2"`
    seconds=`calc "${seconds} * (1 + ($3 / 100))"`
    printf '%6.3f seconds [%s]\n' $seconds $(to_time ${seconds%.*})
}

# computes 1024-based sum of file sizes (default matches all files found in local directory)
function file_sum()
{
    local args=()
    if [[ "$1" = '--ignore-rcs' ]]; then
        shift; args=(\( -name .svn -o -name .git -o -name .hg \) -prune -o)
    fi
    if [[ $# -ne 1 ]]; then
        echo 'usage: file_sum [--ignore-rcs] <path>' >&2
        return 1
    fi
    \find "$1" "${args[@]}" -type f -ls | \
        awk '{i+=1;t+=$7;if($7>l){l=$7;n=$11}};END{if(t>1099511627776){d=1099511627776;u="TiB"}else if(t>1073741824){d=1073741824;u="GiB"}else if(t>1048576){d=1048576;u="MiB"}else if(t>1024){d=1024;u="KiB"}else{d=1;u="B"};printf "found %'"'"'d files using %.2f %s; largest file was %s using %d bytes\n", i, t/d, u, n, l}'
}

# How much did one value change related to another value (e.g. how much changed from A to B)?
function percent_change()
{
    if [ $# -ne 2 ]; then
        echo 'usage: percent_change <number_or_expression> <number_or_expression>' >&2
        return 1
    fi
    calc -p 2 "((($2) - ($1)) / ($1)) * 100"
}

# How much of one value is another (e.g. how much of B is A)?
function percent_of()
{
    if [ $# -ne 2 ]; then
        echo 'usage: percent_of <number_or_expression> <number_or_expression>' >&2
        return 1
    fi
    calc -p 2 "($1) / ($2) * 100"
}

FMFTS='.fmfts'
function find_modfiles()
{
    if [ $# -ne 1 ]; then
        echo 'usage: find_modfiles <directory>' >&2
        return 1
    fi

    local o
    istty stdout && o='-print' || o='-print0'

    local d="$1"; shift
    local f="${d}/${FMFTS}"

    if [ -f "$f" ]; then
        find "$d" -not -name "$f" -type f -newer "$f" $o
    else
        find "$d" -type f $o
    fi
}

function update_modfiles()
{
    if [ $# -lt 1 ]; then
        echo 'usage: update_modfiles <directory> [[CC]YY]MMDDhhmm[.SS]' >&2
        return 1
    fi
    local f="${1}/${FMFTS}"
    test $# -gt 1 && touch -t "$2" "$f" || touch "$f"
}

if ihave docker; then
    function docker_clean()
    {
        local running
        running=$(docker ps -a | awk 'NR>1{print $1}')
        if [[ -n "$running" ]]; then
            echo "kill/rm: $running"
            docker kill $running
            docker rm $running
        fi
        local imgs
        imgs=$(docker images -f dangling=true -q)
        if [[ -n "$imgs" ]]; then
            echo "rm: $imgs"
            docker rmi $imgs
        fi
    }

    function docker_run()
    {
        if [[ $# -lt 1 ]]; then
            echo 'usage: docker_run [opts] <image>' >&2
            return 1
        fi
        docker run --rm -ti -v "${PWD}:/mnt/${PWD##*/}" "$@" bash
    }
fi

if [[ -n "$GOPATH" ]]; then
    function goclone()
    {
        local dst
        eval "$(gitparse "${@: -1}")"
        dst="${GOPATH}/src/${host}/${gituser}/${path}"
        git clone "$@" "$dst" && ln -vsf "$dst" "${WORKDIR}/$(basename "$dst")"
    }

    function gocd()
    {
        local match pn
        IFS=$'\n' match=(
            $(\find "${GOPATH}/src" \
                    \( -name .svn -o -name .git -o -name .hg -o -name vendor \) -prune -o \
                    -follow -type d -iname "*${*}*" -print)
        )

        if [[ ${#match[@]} -lt 1 ]]; then
            echo 'Unable to locate a matching directory' >&2
            return 1
        fi

        if [[ ${#match[@]} -eq 1 ]]; then
            pn="${match[0]}"
        else
            PS3='Choice: '
            select pn in "${match[@]}"; do break; done
        fi

        [[ -z "$pn" ]] && return 2
        cd "$pn"
    }
fi

if ihave bundle; then
    if [[ -z "$CPU_COUNT" ]]; then
        CPU_COUNT=$(grep -cF processor /proc/cpuinfo 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
        export CPU_COUNT
    fi

    export BUNDLE_JOBS=$CPU_COUNT

    function bundle-use-local()
    {
        if [ $# -ne 2 ]; then
            echo 'usage: bundle-use-local <gemname> <local_path>' >&2
            return 1
        fi

        local bn

        bn="$(gitsetbranchname "$2" && echo "$branch_name")"
        sed -i .prev '/'"$1"'/ { n; n; s|^\( *\)[^,]*\(,*\)$|\1branch: "'"$bn"'"\2|; }' Gemfile

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

if ihave gem; then
    function gem_which()
    {
        local gembin='gem'
        if ihave bundle && [ -f Gemfile -o -d '.bundle' ]; then
            if [ -x './bin/bundle' ]; then
                gembin='./bin/bundle exec gem'
            else
                gembin='bundle exec gem'
            fi
        fi
        local out="$(${gembin} which "$@" 2>/dev/null)"
        if [ $? -eq 0 -a -n "$out" ]; then
            echo "$out"
            return 0
        fi
        local name
        local rc=1
        for name in $(${gembin} list | awk '/'"$1"'/{print $1}'); do
            local out="$(${gembin} which "$name" 2>/dev/null)"
            if [ $? -eq 0 -a -n "$out" ]; then
                echo "$out"
                rc=0
            fi
        done
        return $rc
    }
fi

# Search a rails log for matches and include stack traces reported in between matches.
# (also a good example for how awk can search across multiple lines watching for a terminating pattern)
function rlog_awk()
{
    if [ $# -ne 2 ]; then
        echo 'usage: rlog_awk <regexp> <rails_log_file>' >&2
        return 1
    fi
    # Expects that each rails log line starts like this: I, [2016-02-18T21:16:35.913665 #12726]
    awk '/'"$1"'/{f=1;print;next}f&&/^., /{f=0;next}f{print}' "$2"
}

function rails_stackprof()
{
    if [ $# -ne 2 ]; then
        echo 'usage: rails_stackprof <prep_ruby> <prof_ruby>' >&2
        return 1
    fi
    rails runner 'require "stackprof";'"$1"';StackProf::Report.new(StackProf.run{'"$2"'}).print_text'
}

if ihave virtualenv; then
    # TODO: support .venv file like rvm that automatically activates on entering a directory
    VENV_PATH="${HOME}/.local/share/virtualenvs" # shared with pipenv
    [[ -d "$VENV_PATH" ]] || mkdir -p "$VENV_PATH"

    function venv_list()
    {
        ls -ald "${VENV_PATH}"/*
    }

    function venv_remember()
    {
        local args=()
        if [[ "$1" = '--use' ]]; then
            shift; args+=(-p "$1"); shift;
        fi
        local g="$1"
        [ -n "$g" ] || g="$(git remote -v | sed 's/^.*\/\(.*\).git.*$/\1/;q')"
        [ -d "${VENV_PATH}/$g" ] || virtualenv "${args[@]}" "${VENV_PATH}/$g"
        source "${VENV_PATH}/${g}/bin/activate"
        if $DARWIN; then
           if ! codesign -dv "$(which python)" 2>&1 | grep -qF 'Internal requirements count=0'; then
               # allows things like keyring to work
               codesign -f -s - "$(which python)"
           fi
        fi
    }

    function venv_forget()
    {
        local g
        if [ $# -eq 1 ]; then
            g="${VENV_PATH}/${1}"
        elif ihave deactivate; then
            g="$VIRTUAL_ENV"
            deactivate
        else
            g="${VENV_PATH}/$(git remote -v | sed 's/^.*\/\(.*\).git.*$/\1/;q')"
        fi
        \rm -rf "$g"
    }

    PY2_BINPATH="${VENV_PATH}/py2/bin"
    PY2_GLOBALS='pip kubey collabi garbagetruck grip boto Mercurial mercurial_keyring'
    function py2_update_globals()
    {
        local pkg pkgbin
        for pkg in ${PY2_GLOBALS}; do
            pkgbin="${PY2_BINPATH}/${pkg}"
            if [[ -e "${pkgbin}" ]]; then
                "${PY2_BINPATH}/pip" install -U "${pkg}"
            else
                "${PY2_BINPATH}/pip" install "${pkg}" &&
                    ln -s "${pkgbin}" "${HOME}/bin/${pkg}"
            fi
        done
    }

    function py2_install()
    {
        local tf=$(mktemp)
        "${PY2_BINPATH}/pip" install "$1" &&
            sed "s/^\( *PY2_GLOBALS=.*\)'\$/\1 $1'/" "${BASH_SOURCE[0]}" > "${tf}" &&
            cp "${tf}" "${BASH_SOURCE[0]}"
        rm -f "${tf}"
        [[ -e "${PY2_BINPATH}/$1" ]] && ln -s "${PY2_BINPATH}/$1" "${HOME}/bin/${1}"
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

        function rvm_update_all()
        {
            rvm_update default || return

            for v in $(rvm list rubies | sed -n 's/^.*\(ruby-[^ ]*\).*$/\1/p'); do
                rvm_update "${v}@global" \
                           pry pry-byebug pry-doc file_discard bundler ssh-config \
                           better_bytes filecamo colorize || return
            done
        }

        function rvm_update()
        {
            if [[ $# -lt 1 ]]; then
                echo 'usage: rvm_update <ruby_ver_gemset> [<gem> ...]' >&2
                return 1
            fi

            (
                export GEM_PATH=$GEM_HOME # Force actions only to this gemset's gems!
                set -e
                rv=$1; shift
                rvm use "$rv"
                gem update
                [[ $# -gt 0 ]] && gem install "$@"
                a="RVM_PIN_$(varfrom "$rv")[@]"
                echo "${GREEN}Looking for pins in ${a}${NORMAL}"
                for pin in "${!a}"; do
                    nv=(${pin//:/ })
                    if [[ ${#nv[@]} -ne 2 ]]; then
                        echo "failed to parse pin: ${pin}"
                        return 1
                    fi
                    gem uninstall "${nv[0]}" -v "> ${nv[1]}" -x || :
                    gem install "${nv[0]}" -v "${nv[1]}"
                done
                gem clean
            )
        }

        function rvm_list_only_current()
        {
            GEM_PATH=$GEM_HOME gem_list_installed
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

# Some EC2 instances will use tags to indicate environment settings to web frontends.
if [ -n "$EC2_ENV" ]; then
    export RAILS_ENV="$EC2_ENV"
    export NODE_ENV="$EC2_ENV"
    ihave awsenv && awsenv "$EC2_ENV" >/dev/null 2>&1
fi


########################
$INTERACTIVE || return 0
########################


# Execution
# #########

if $IAMME; then
    if ! ihave ag && [ -d "${HOME}/bin" ] && [ ! -x "${HOME}/bin/spot" ]; then
        # File content search tool
        # TODO make this work with httpget! curl isn't always installed (e.g. ubuntu)
        echo 'Downloading "spot" search tool to bin...'
        curl -sfSL https://raw.githubusercontent.com/rauchg/spot/master/spot.sh -o "${HOME}/bin/spot" && \
            chmod +x "${HOME}/bin/spot"
    fi

    if $IAMME && test -d "${HOME}/.ssh"; then
        [ -f "${HOME}/.ssh/config" ] || reload_ssh_config
    fi

    if [ -z "$SSH_CLIENT" ]; then
        # Only run an ssh-agent on a local machine...not when logged in remotely via SSH or sume'd.

        . "${HOME}/.ssh/agent_env.sh" >/dev/null 2>&1
        if test -z "$SSH_AGENT_PID" || ! pgrep ssh-agent | grep -qF "$SSH_AGENT_PID"; then
            ssh-agent > "${HOME}/.ssh/agent_env.sh"
            printf 'New SSH '
            . "${HOME}/.ssh/agent_env.sh"
        fi

        # NOTE: each entry MUST be ended with a newline (esp the last line!)
        if [ -f "${HOME}/.ssh/ssh_agent.keys" ]; then
            while read -r key; do
                # expand tilde and other variables (e.g. $HOME)
                __expand_tilde_by_ref key
                eval "key=\"${key}\""
                ssh-add "$key" >/dev/null 2>&1
            done < <(cat "${HOME}/.ssh/ssh_agent.keys")
        fi
    fi

    mkdir -p "${HOME}/.ssh/cm" # used by ssh config ControlPath
    chmod 700 "${HOME}/.ssh/cm"

    # On OS X, try to run cmd-key-happy to have option and command conditionally remapped.
    if $DARWIN && ihave cmd-key-happy; then
        if ! pgrep cmd-key-happy >/dev/null; then
            nohup cmd-key-happy >/dev/null 2>&1 </dev/null &
        fi
    fi

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
alias ps='psgrep'
alias which='btwhich'
alias ssh='retitlessh'

ihave discard && alias rm=discard || :
ihave colordiff && alias diff=colordiff || :

test -n "$SIMPLE_PROMPT" && simplify_prompt

# set final return code as "success"
true
