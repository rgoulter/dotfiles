let g:hip_location = "~/hg/sleekex/hip"

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

" For a .ss Hip/Sleek file,
" opens a new split, with the output vert.
function! HipRunSS()
    " Run the current file through Hip
    let fn = expand("%:p")
    let cmd = g:hip_location . ' ' . fn
    let output = system(cmd)

    " (Vert) split with a new scratch buffer,
    " dump the output there
    vnew
    set buftype=nofile
    call append(0, split(output, "\n"))

    " Go to start of file
    normal gg
endfunction

augroup hipsleekdev
    " A keybinding for *.t2 files to call the above.
    autocmd BufNewFile,BufRead *.t2 nnoremap <buffer> <localleader>r :call Nrunont2input()<CR>

    " ss is our hip/slk, NOT scheme
    au BufNewFile,BufRead *.ss set filetype= 

    " A keybinding for *.ss files to call the above.
    autocmd BufNewFile,BufRead *.ss nnoremap <buffer> <localleader>r :call HipRunSS()<CR>
augroup END
