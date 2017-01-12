# These are needed for allowing git aliases to use particular completion methods.
# See https://coderwall.com/p/d2w7oa/auto-completion-within-complex-git-alias for details

# git co <branch> (i.e. checkout)
_git_co() { _git_branch; }
