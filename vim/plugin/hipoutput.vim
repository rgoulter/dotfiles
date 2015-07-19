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

" For a .ss Hip/Sleek file,
" opens a new split, with the output vert.
function! HipRunSS()
    " Run the current file through Hip
    let fn = expand("%:p")
    let cmd = g:hip_location . ' ' . fn
    let output = system(cmd)

    " (Vert) split with a new scratch buffer,
    " dump the output there
    " TODO: We want to be (re)using *the same* buffer each time;
    " or, at least, the same "window", if it exists.
    vnew
    set buftype=nofile
    call append(0, split(output, "\n"))

    " Go to start of file
    normal gg
endfunction

function! IsInHipDir()
    let cwd = expand("%:p:h")

    if cwd =~ ("^" . expand(g:hip_dir))
        " The current file is in `hip_dir'.
        " Do some stuff
        " (This may be more suited to `filetype` .vim file?
        "  and/or "local" per-project/per-directory vimrc files?).
        compiler ocamlbuild
    endif
endfunction

augroup hipsleekdev
    " For OCaml files, want to set the compiler if it's in
    " the project directory.
    autocmd BufNewFile,BufRead *.ml call IsInHipDir()

    " A keybinding for *.t2 files to call the above.
    autocmd BufNewFile,BufRead *.t2 nnoremap <buffer> <localleader>r :call Nrunont2input()<CR>

    " A keybinding for *.ss files to call the above.
    autocmd BufNewFile,BufRead *.ss nnoremap <buffer> <localleader>r :call HipRunSS()<CR>
augroup END
