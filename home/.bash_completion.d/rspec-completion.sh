_rspec_completion()
{
    local curr_arg avail
    curr_arg=${COMP_WORDS[COMP_CWORD]}
    avail=(`find spec -type f -name '*_spec.rb'`)
    [ -z "$avail" ] && return 1
    COMPREPLY=($(compgen -W "${avail[*]}" -- $curr_arg))
}

complete -F _rspec_completion rspec
