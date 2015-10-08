" Vim syntax file
" Language:	Sleek
" Maintainer:	Richard Goulter <richard.goulter@gmail.com>
" Last Change:	Under development

" Little care has been taken to make this;
" it's more as-I-need the feature, rather than comprehensive

if exists("b:current_syntax")
  finish
endif

syntax region hsoutpLog start="!!!" end="#[0-9]\+:"

"" The Sleek syntax stuff should only apply within the Sleek region
syntax region hsoutpExprReg start="\[" end="\]" contains=@SleekCluster
syn cluster SleekCluster contains=hsoutpExprReg
" syn cluster SleekCluster add=hsoutpParen1
syn cluster SleekCluster add=hsoutpParen2
syn cluster SleekCluster add=hsoutpParen3
syn cluster SleekCluster add=hsoutpParen4
syn cluster SleekCluster add=hsoutpHeapOp
syn cluster SleekCluster add=hsoutpKeyword
syn cluster SleekCluster add=hsoutpNumber
syn cluster SleekCluster add=hsoutpIdent

syntax keyword hsoutpKeyword EList EBase
syntax keyword hsoutpKeyword self view emp null exists node FLOW __flow

"" Since these are the same symbols as above, I'm not sure how to deal with
"" them.
" syntax match hsoutpParen1 "\["
" syntax match hsoutpParen1 "\]"

syntax match hsoutpParen2 "(\*" contained
syntax match hsoutpParen2 "*)" contained
syntax match hsoutpParen3 "([^*]\@=" contained
syntax match hsoutpParen3 ")" contained
syntax match hsoutpParen4 "{" contained
syntax match hsoutpParen4 "}" contained
syntax match hsoutpParen4 "<" contained
syntax match hsoutpParen4 ">" contained

syntax match hsoutpHeapOp "::" contained
syntax match hsoutpHeapOp "@" contained
syntax match hsoutpHeapOp "->" contained
syntax match hsoutpHeapOp "=" contained
syntax match hsoutpHeapOp "#" contained
syntax match hsoutpHeapOp "&" contained
syntax match hsoutpHeapOp "\*[^)]\@=" contained
" syntax region sleekComment start=/\/\*/ end=/\*\//

syntax match hsoutpNumber "[0-9]\+" contained
syntax match hsoutpIdent "[_a-zA-Z][_a-zA-Z0-9]*" contained



" Again, not ideal, but since [] are used within, also.
highlight hsoutpExprReg ctermfg=DarkCyan

highlight hsoutpKeyword ctermfg=Cyan

highlight hsoutpHeapOp ctermfg=Blue

highlight hsoutpLog ctermfg=DarkGreen

" highlight hsoutpParen1 ctermfg=DarkCyan
highlight hsoutpParen2 ctermfg=DarkRed
highlight hsoutpParen3 ctermfg=DarkMagenta
highlight hsoutpParen4 ctermfg=Brown

highlight hsoutpIdent ctermfg=Yellow
highlight hsoutpNumber ctermfg=DarkGray

let b:current_syntax = "hipsleek-output.vim"
