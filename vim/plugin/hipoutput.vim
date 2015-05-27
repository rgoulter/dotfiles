" Could be a macro, since this statefully/destructively modifies the cursor.
" Alternatively, could try and write this up as "pure" calls
"  to VimL functions instead.
function! Ntidyhipoutput()
    normal gg
    execute "/void main"
    normal V}=
    " Remove things before/after this block.
    normal }dG
    " Two {{ keeps the globals.. one { for just main.
    normal {{dgg
endfunction
