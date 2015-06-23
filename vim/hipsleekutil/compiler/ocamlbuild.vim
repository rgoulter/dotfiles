" Vim Compiler File
" for ocamlbuild project.
" Compiler:	ocaml
" Maintainer:	See ftplugin/ocaml.vim (?)
" Last Change:	June 2013 by Marc Weber

" I'm unsure of what ocamlbuild output is in general;
" but this wfm.

if exists("current_compiler")
    finish
endif
let current_compiler = "ocamlbuild"

let s:cpo_save = &cpo
set cpo&vim

CompilerSet errorformat =
      \%AFile\ \"%f\"\\,\ line\ %l\\,\ characters\ %c-%*\\d:,
      \%AFile\ \"%f\"\\,\ line\ %l\\,\ character\ %c:%m,
      \%C%tarning\ %n:\ %m,
      \%C%trror\ %n:\ %m,
      \%-G/usr%.%#,
      \%-G+\ /usr%.%#,
      \%-G#\ No\ parallelism\ done,
      \%-Gcp%.%#,
      \%C%m

let &cpo = s:cpo_save
unlet s:cpo_save
