" Try to find hip dir.
function! FindHipSleekRepo()
    let cwd = expand("%:p:h")

    " use !empty(glob(V)) to check if V exists

    " Look in parents, until find .hg repo.
    " strlen > 1 is check against root..
    while empty(glob(cwd . "/.hg")) && strlen(cwd) > 1
        let cwd = cwd . "/.."
    endwhile

    let cwd = simplify(cwd)

    " if cwd == /, then probably fail..
    echom "Found repo dir: " . cwd

    return cwd
endfunction

let g:hip_dir = FindHipSleekRepo()
let g:sleek_location = g:hip_dir . "/sleek"
let g:hip_location = g:hip_dir . "/hip"

" Local to script.
let s:buf_id = -1

" For a .slk Hip/Sleek file,
" opens a new split, with the output vert.
function! SleekRun()
    " Run the current file through Hip
    let fn = expand("%:p")
    if exists("b:extra_sleek_args")
      let cmd = g:sleek_location . ' ' . fn . ' ' . b:extra_sleek_args
    else
      let cmd = g:sleek_location . ' ' . fn
    end
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

nnoremap <buffer> <localleader>r :call SleekRun()<CR>

"" Not sure the best way to do this.
"" ?? Proj specific vimrc ?
" let b:extra_sleek_args=""
