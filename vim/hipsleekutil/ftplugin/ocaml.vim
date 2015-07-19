" For OCaml files, want to set the compiler if it's in
" the project directory.

let g:hip_dir = "~/hg/sleekex"


let cwd = expand("%:p:h")

if cwd =~ ("^" . expand(g:hip_dir))
    " The current file is in `hip_dir'.
    " Do some stuff
    " (This may be more suited to `filetype` .vim file?
    "  and/or "local" per-project/per-directory vimrc files?).
    compiler ocamlbuild
endif
