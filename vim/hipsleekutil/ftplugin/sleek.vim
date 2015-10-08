" let g:hip_dir = "~/hg/sleekex"
let g:hip_dir = "~/hg/fix-derive"
let g:sleek_location = g:hip_dir . "/sleek"
let g:hip_location = g:hip_dir . "/hip"

" Local to script.
let s:buf_id = -1

" For a .slk Hip/Sleek file,
" opens a new split, with the output vert.
function! SleekRun()
    " Run the current file through Hip
    let fn = expand("%:p")
    let cmd = g:sleek_location . ' ' . fn
    let output = system(cmd)

    " Try to reuse buffer / window, if they exist
    if s:buf_id < 0
        " (Vert) split with a new scratch buffer,
        " dump the output there
        " TODO: We want to be (re)using *the same* buffer each time;
        " or, at least, the same "window", if it exists.
        vnew
        set buftype=nofile
        set filetype=hipsleek-output

        let s:buf_id = bufnr('%')
    else
        let outp_win = bufwinnr(s:buf_id)

        if outp_win >= 0
            " window exists, buffer exists

            "" Need to operate on that buffer / switch to that window
            "" couldn't get "wincmd #" to work
            execute outp_win . "wincmd w"
        else
            " buf exists, but no window for it

            "" Need to create window / switch to that.
            "" ?? How to check for best way to split??
            vsplit
            execute "buffer " . s:buf_id
        end

        " Clear buffer contents
        normal ggdG
    endif

    call append(0, split(output, "\n"))

    " Go to start of file
    normal gg
endfunction

" nnoremap <buffer> <localleader>r :call HipRunSS()<CR>
nnoremap <buffer> <localleader>r :call SleekRun()<CR>
