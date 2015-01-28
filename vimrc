set nocompatible               " be iMproved
filetype off                   " required! by Vundle

" Because we use Fish shell.
" see http://stackoverflow.com/questions/12230290/vim-errors-on-vim-startup-when-run-in-fish-shell
set shell=/bin/sh

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

" let Vundle manage Vundle
" Vundle is the way you want to handle Vim plugins.
Plugin 'gmarik/vundle'

" My Bundles here:

" YouCompleteMe replaces AutoComplPop, and other things,
"  to provide modern completion capabilities in Vim.
" Requires to be recompiled when it updates.
" C-family code intelligence is a bit more complicated to install.
Plugin 'Valloric/YouCompleteMe'

" Tern for Vim is for intelligent JavaScript code completion, etc.
Plugin 'marijnh/tern_for_vim'

" Ultisnips is a snippets plugin for Vim.
Plugin 'ervandew/supertab'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

" TagBar provides nice code-browsing / structure overview.
"  Needs exuberant ctags.
Plugin 'majutsushi/tagbar'

Plugin 'epeli/slimux'

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
Plugin 'altercation/vim-colors-solarized'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'tomasr/molokai'

" All of your Plugins must be added before the following line
call vundle#end()            " required by Vundle
filetype plugin indent on    " required by Vundle



syntax enable
set background=dark

set hlsearch    " Enable Search Highlighting
set incsearch   " Enable search while typing
set showmatch   " Show matching brackets
set ignorecase
set smartcase   " Be smart about cases when searching

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
set tabstop=4
set expandtab
set autoindent  " Enables Auto Indent on files without type

set colorcolumn=80

" Have backspace work properly.
set backspace=eol,start,indent

set encoding=utf-8
set list listchars=tab:→\ ,trail:·

" Disable swapfile, backup since we shouldn't need them.
set noswapfile
set nobackup

set foldmethod=syntax
set foldlevelstart=5

" Tips from http://blog.sanctum.geek.nz/vim-annoyances/
" Replace all occurrences during substititon (s/pat/repl/) by default; adding
" g will keep it to the first occurrence
set gdefault
" Better defaults for new splits:
"set splitbelow
set splitright

" To help CtrlP ignore files
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.o,*.hi,*.dyn_o



" Tips from http://blog.sanctum.geek.nz/vim-annoyances/
" Regardling j/k over wrapped lines
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k


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



" .md as Markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.markdown set filetype=markdown



" Terminal Colours
color solarized
let g:solarized_termcolors=16
let g:solarized_termtrans=0



" Haskell

" In ~/.vim/ftplugin/haskell.vim
" setlocal omnifunc=necoghc#omnifunc

" For getting neco-ghc working with YouCompleteMe
let g:ycm_semantic_triggers = {'haskell' : ['.']}

"g:necoghc_enable_detailed_browse = 1

au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsClear<CR>



" OCaml

" Add merlin to vim's runtime-path:
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"

" Also run the following line in vim to index the documentation:
"   :execute "helptags " . g:opamshare . "/merlin/vim/doc"

if filereadable("$HOME/.opam/4.02.1/share/vim/syntax/ocp-indent.vim")
    execute ":source " . "$HOME/.opam/4.02.1/share/vim/syntax/ocp-indent.vim"
endif



" Have YouCompleteMe and eclim
"  play nicely with each other.
let g:EclimCompletionMethod = 'omnifunc'



let g:airline_powerline_fonts=1



" YouCompleteMe and Ultisnips,
" As per (last updated Jan 22 2014)
" http://stackoverflow.com/questions/14896327/ultisnips-and-youcompleteme

" make YCM compatible with UltiSnips (using supertab)
" As per supertab, use c-n, c-p to cycle autocomplete; use tab for Ultisnips
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"



" Let's improve YCM
let g:ycm_complete_in_comments = 1
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_collect_identifiers_from_tags_files = 1



" For the Sessions plugin
" Session autosaving is tedious.
let g:session_autosave = 'no'



" For slimux
map <CR> :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
map <Leader>a :SlimuxShellLast<CR>
map <Leader>k :SlimuxSendKeysLast<CR>



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
