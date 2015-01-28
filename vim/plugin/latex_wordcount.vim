" For LaTeX Wordcount
" acquired from http://tex.stackexchange.com/questions/534/is-there-any-way-to-do-a-correct-word-count-of-a-latex-document
function! WC()
    let filename = expand("%")
    let cmd = "detex " . filename . " | wc -w | tr -d [:space:]"
    let result = system(cmd)
    echo result . " words"
endfunction
