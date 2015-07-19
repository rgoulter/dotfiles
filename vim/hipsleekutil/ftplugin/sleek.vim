let g:hip_dir = "~/hg/sleekex"
let g:hip_location = g:hip_dir . "/hip"

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

nnoremap <buffer> <localleader>r :call HipRunSS()<CR>
