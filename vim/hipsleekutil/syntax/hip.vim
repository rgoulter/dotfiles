" Vim syntax file
" Language:	Sleek
" Maintainer:	Richard Goulter <richard.goulter@gmail.com>
" Last Change:	Under development

if exists("b:current_syntax")
  finish
endif

syntax keyword sleekDataDecl data
syntax keyword sleekSelf self

syntax keyword sleekPredTerm or
syntax keyword sleekPredTerm inv

syntax keyword sleekContract requires ensures

syntax region sleekComment start=/\/\*/ end=/\*\//

highlight link sleekDataDecl Type
highlight link sleekSelf Identifier

highlight link sleekPredTerm Operator
highlight link sleekContract Operator

highlight link sleekComment Comment

let b:current_syntax = "hip"
