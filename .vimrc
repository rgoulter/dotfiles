set nocompatible               " be iMproved
filetype off                   " required!

" Because we use Fish shell.
" see http://stackoverflow.com/questions/12230290/vim-errors-on-vim-startup-when-run-in-fish-shell
set shell=/bin/sh

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#rc()

" let Vundle manage Vundle
" Vundle is the way you want to handle Vim plugins.
Plugin 'gmarik/vundle'

" My Bundles here:

" YouCompleteMe replaces AutoComplPop, and other things,
"  to provide modern completion capabilities in Vim.
" Requires to be recompiled when it updates.
" C-family code intelligence is a bit more complicated to install.
Plugin 'Valloric/YouCompleteMe'
"Plugin 'Shougo/neocomplete.vim'
" Tern for Vim is for intelligent JavaScript code completion, etc.
Plugin 'marijnh/tern_for_vim'
" Ultisnips is a snippets plugin for Vim.
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

" TagBar provides nice code-browsing / structure overview.
"  Needs exuberant tags.
Plugin 'majutsushi/tagbar'

Plugin 'epeli/slimux'

" Powerline is a pretty powerline to have.
" Requires fancy fonts to look super-pretty.
"Plugin 'Lokaltog/powerline'

" vim-airline is a lightweight alternative to Powerline.
Plugin 'bling/vim-airline'

Plugin 'scrooloose/syntastic'

" Haskell
" Opting for ghcmod-vim over hdevtools, as this is what vim2hs supports.
"  Plugin 'bitc/vim-hdevtools'
" Opting for vim2hs, haskellmode-vim hasn't been updated in a long time.
"  Plugin 'lukerandall/haskellmode-vim'
" ghcmod-vim depends on vimproc.vim. n.b. vimproc needs to be built natively.
Plugin 'Shougo/vimproc.vim'
Plugin 'eagletmt/ghcmod-vim'
Plugin 'dag/vim2hs'
" nego-ghc for haskell autocompletion. Can work with YCM, see below.
" But also requires
"   setlocal omnifunc=necoghc#omnifunc 
" e.g. in ~/.vim/ftplugin/haskell.vim
Plugin 'eagletmt/neco-ghc'
" Lushtags for Haskell+Ctags.
" See https://github.com/zenzike/vim-haskell for discussion of alternatives.
Plugin 'bitc/lushtags'
Plugin 'travitch/hasksyn'

" PHP, + HTML + JS
" Updated PHP syntax
Plugin 'StanAngeloff/php.vim'
" Improved PHP indenting.
Plugin '2072/PHP-Indenting-for-VIm'

" tComment for comments.
" (Alternative is NERDCommenter).
Plugin 'tomtom/tcomment_vim'

" Ctrl-P for fuzzy-finding of opening files.
" (Replacing FuzzyFinder).
Plugin 'kien/ctrlp.vim'

" a la ST, mulitcursor support. Looks cool.
Plugin 'terryma/vim-multiple-cursors'

" For +/- for diff from VCS.
Plugin 'mhinz/vim-signify'

" Uses Ack as an improvement over grep searching.
Plugin 'mileszs/ack.vim'

" Some good bindings by Tim Pope to help with
" quickfix and stuff
Plugin 'tpope/vim-unimpaired'

" Because why use vanilla sessions?
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-session'
Plugin 'xolox/vim-easytags'
Plugin 'xolox/vim-notes'

" For matching files.
Plugin 'vim-scripts/a.vim'

" Plugins for improved language support
Plugin 'sukima/xmledit'
Plugin 'elzr/vim-json'

" VCS (HG, Git) Plugins for Vim
Plugin 'tpope/vim-fugitive'
Plugin 'phleet/vim-mercenary'
Plugin 'vim-scripts/Lawrencium'

" Distraction free writing
" Use :Goyo or :Goyo 80,
" As well as Limelight [0.0, 1.0], Limelight!!
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'

" For easy alignment
Plugin 'junegunn/vim-easy-align'

" Color Schemes
" Solarized Color Scheme.
Plugin 'altercation/vim-colors-solarized'
" Tomorrow Theme(s) for vim Color Schemes.
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'tomasr/molokai'

" Mac OSX only..
if has("unix")
  let s:uname = system("uname -s")
  if s:uname == "Darwin"
    " Do Mac stuff here
    Plugin 'eraserhd/vim-ios'
  endif
endif

" vim-scripts repos

" L9 is needed for some things, I think.
Plugin 'L9'
" FuzzyFinder is a really cool way to open files,
"  so that you can open files with just a subsequence.
"Plugin 'FuzzyFinder'

" Required for Vundle, and many other things:
filetype plugin indent on

syntax enable
set background=dark

" Terminal
"color Tomorrow-Night
let g:solarized_termcolors=16
let g:solarized_termtrans=0
color solarized
let g:solarized_termcolors=16
let g:solarized_termtrans=0

set hlsearch    " Enable Search Highlighting
set incsearch   " Enable search while typing
set showmatch   " Show mating brackets
set ic          " Ignore Case
set smartcase   " Be smart about cases.

"set cmdheight=2 " Height of Command bar.
set cmdheight=2 " Height of Command bar.
set showmode    " Shows mode
set showcmd     " Shows cmd
set number      " Enable Line Number
set ruler       " Enable Ruler

" For vim airline
set laststatus=2

"" Indentation Settings
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent  " Enables Auto Indent on files without type
filetype plugin on
filetype indent on

" Column count of 80
set cc=80

" Have backspace work properly.
set backspace=eol,start,indent

" Some nice convenient keys.
"nmap <F3> :FufCoverageFile<CR>
" For "Quick Load File", using CtrlP
nmap <F3> :CtrlPMixed<CR>
" Change whether to show hidden characters or not.
nmap <F4> :set list!<CR>
" Show the Tagbar "Code Overview"
nmap <F8> :TagbarToggle<CR>

nmap <F9> :ProjectTreeToggle<CR>

" Opposite of Shift-J,
" from http://vim.wikia.com/wiki/Insert_newline_without_entering_insert_mode
nnoremap <C-J> a<CR><Esc>k$

" Haskell

" In ~/.vim/ftplugin/haskell.vim
" setlocal omnifunc=necoghc#omnifunc

" Haskell, load Haddock using Chrome
let g:haddock_browser="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"


" For getting neco-ghc working with YouCompleteMe
let g:ycm_semantic_triggers = {'haskell' : ['.']}

"g:necoghc_enable_detailed_browse = 1

au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>


" Have YouCompleteMe and eclim
"  play nicely with each other.
let g:EclimCompletionMethod = 'omnifunc'

" Powerline
"set rtp+=~/github/powerline/powerline/bindings/vim
"set laststatus=2 " Always display the statusline in all windows
"set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

let g:airline_powerline_fonts=1


" YouCompleteMe and Ultisnips,
" See: http://stackoverflow.com/questions/14896327/ultisnips-and-youcompleteme
" Don't see; deprecated.
" function! g:UltiSnips_Complete()
"     call UltiSnips_ExpandSnippet()
"     if g:ulti_expand_res == 0
"         if pumvisible()
"             return "\<C-n>"
"         else
"             call UltiSnips_JumpForwards()
"             if g:ulti_jump_forwards_res == 0
"                return "\<TAB>"
"             endif
"         endif
"     endif
"     return ""
" endfunction
"
" func! g:jInYCM()
"     if pumvisible()
"         return "\<C-n>"
"     else
"         return "\<c-j>"
" endfunction
"
" func! g:kInYCM()
"     if pumvisible()
"         return "\<C-p>"
"     else
"         return "\<c-k>"
" endfunction
inoremap <c-j> <c-r>=g:jInYCM()<cr>
au BufEnter,BufRead * exec "inoremap <silent> " . g:UltiSnipsJumpBackwordTrigger . " <C-R>=g:kInYCM()<cr>"
let g:UltiSnipsJumpBackwordTrigger = "<c-k>"

" au BufEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger . " <C-R>=g:UltiSnips_Complete()<cr>"
"let g:UltiSnipsExpandTrigger="<tab>"
"let g:UltiSnipsJumpForwardTrigger="<tab>"
" let g:UltiSnipsExpandTrigger="<c-tab>"
" let g:UltiSnipsListSnippets="<c-s-tab>"
let g:UltiSnipsExpandTrigger="<c-j>"
let g:UltiSnipsJumpForwardTrigger="<c-j>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"


" Tips from http://blog.sanctum.geek.nz/vim-annoyances/
" Regardling j/k over wrapped lines
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k
" Replace all occurrences during substititon (s/pat/repl/) by default; adding
" g will keep it to the first occurrence
set gdefault
" Better defaults for new splits:
"set splitbelow
set splitright


set encoding=utf-8
set list listchars=tab:→\ ,trail:·
 
" This might be a bad idea, but..
set tabstop=4

" By default, hide menu + toolbar..
set go-=m
set go-=T

" Disable swapfile, backup since we shouldn't need them.
set noswapfile
set nobackup

" For the Sessions plugin
let g:session_autosave = 'no'

set foldmethod=syntax
set foldlevelstart=5

" For LaTeX Wordcount
" acquired from http://tex.stackexchange.com/questions/534/is-there-any-way-to-do-a-correct-word-count-of-a-latex-document
function! WC()
    let filename = expand("%")
    let cmd = "detex " . filename . " | wc -w | tr -d [:space:]"
    let result = system(cmd)
    echo result . " words"
endfunction

" Let's improve YCM
let g:ycm_complete_in_comments = 1
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_collect_identifiers_from_tags_files = 1


"Plugin 'Rip-Rip/clang_complete'

" Disable auto completion, always <c-x> <c-o> to complete
let clang_complete_loaded = 1
let g:clang_complete_auto = 1 
let g:clang_use_library = 1
let g:clang_periodic_quickfix = 0
let g:clang_close_preview = 1

" For Objective-C, this needs to be active, otherwise multi-parameter methods won't be completed correctly
let g:clang_snippets = 1

" Snipmate does not work anymore, ultisnips is the recommended plugin
let g:clang_snippets_engine = 'ultisnips'
let g:clang_library_path = "/Library/Developer/CommandLineTools/usr/lib/"

" for debugging YCM
let g:ycm_server_use_vim_stdout = 1
let g:ycm_server_log_level = 'debug'

" For slimux
map <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
map <Leader>a :SlimuxShellLast<CR>
map <Leader>k :SlimuxSendKeysLast<CR>

" .md as Markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.markdown set filetype=markdown

" Distraction free writing,
" Integrate Goyo with Limelight
function! GoyoBefore()
  Limelight
endfunction

function! GoyoAfter()
  Limelight!
endfunction

let g:goyo_callbacks = [function('GoyoBefore'), function('GoyoAfter')]



" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. <Leader>aip)
nmap <Leader>a <Plug>(EasyAlign)

" To help CtrlP ignore files
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.o,*.hi,*.dyn_o
