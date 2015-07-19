let g:hip_dir = "~/hg/sleekex"
let g:hip_location = g:hip_dir . "/hip"

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

    " Remove the control-path labels. (I don't find them useful?).
    " 'Redundant' if formatted..
    silent! execute "%s/(\\d*, )://"

    " Tidy up the empty 'else' clauses
    silent! execute "%s/{\\s*\\n\\s*\\n\\s*/{/"
endfunction

" Execute this in a t2 file
function! Nrunont2input()
    let fn = expand("%:p")
    let cmd = g:hip_location . ' ' . fn . ' --pip'

    let output = system(cmd)

    let fdir = expand("%:p:h")
    let outfn  = fdir . "/" . expand("%:t") . "_out.txt"

    " 'Destructively' edit this file,
    execute ":edit " . outfn
    call append(0, split(output, "\n"))
    call Ntidyhipoutput()

    " Atm I have a guard against committing.
    " May make more sense to just write output to different location.
    call append(0, "# NOCOMMIT")

    write
endfunction

nnoremap <buffer> <localleader>r :call Nrunont2input()<CR>
