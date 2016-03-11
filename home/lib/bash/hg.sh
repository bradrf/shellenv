function hg_root()
{
    local dir="$(pwd)"
    while [ "$dir" != '/' ]; do
      if [ -d "${dir}/.hg" ]; then
          echo "$dir"
          return
      fi
      dir="$(dirname "$dir")"
    done
    return 1
}

function hg_branch()
{
    local root="$(hg_root)"
    test $? -ne 0 && return 1
    cat "${root}/.hg/branch" 2>/dev/null
}
