set nocompatible               " be iMproved
filetype off                   " required! by Vundle

" Because we use Fish shell.
" see http://stackoverflow.com/questions/12230290/vim-errors-on-vim-startup-when-run-in-fish-shell
set shell=/bin/sh

if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

" let Vundle manage Vundle
" Vundle is the way you want to handle Vim plugins.
Plugin 'gmarik/vundle'

" My Bundles here:

""""""""""""""""""""""""""""""""""""""""""
" Plugins which change the way vim looks "
""""""""""""""""""""""""""""""""""""""""""

" vim-airline is a lightweight alternative to Powerline.
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" Make our parentheses, etc. pretty
Plugin 'luochen1990/rainbow'

" For +/- for diff from VCS.
Plugin 'mhinz/vim-signify'

Plugin 'scrooloose/syntastic'

" Color Schemes
Plugin 'altercation/vim-colors-solarized'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'tomasr/molokai'
Plugin 'romainl/flattened'

"""""""""""""""""""""""""""""""""""""""""""""
" Plugins which add normal-mode keybindings "
"""""""""""""""""""""""""""""""""""""""""""""

" More motions
" Like f, but two letters
Plugin 'goldfeld/vim-seek'
" seek works only on one line; sneak is more versatile.
" Plugin 'justinmk/vim-sneak'
" I don't use easy-motion, but nice to have in Vimrc?
Plugin 'easymotion/vim-easymotion'

" a la ST, mulitcursor support. Looks cool.
Plugin 'terryma/vim-multiple-cursors'

" Some good bindings by Tim Pope to help with
" quickfix and stuff
Plugin 'tpope/vim-unimpaired'

Plugin 'tpope/vim-surround'

" For easy alignment
Plugin 'junegunn/vim-easy-align'

" tComment for comments.
" (Alternative is NERDCommenter).
Plugin 'tomtom/tcomment_vim'

" Ctrl-P for fuzzy-finding of opening files.
" (Replacing FuzzyFinder).
Plugin 'kien/ctrlp.vim'



""""""""""""""""""""""""""""""""""""""""""""""
" Plugins which interact with external tools "
""""""""""""""""""""""""""""""""""""""""""""""

" YouCompleteMe replaces AutoComplPop, and other things,
"  to provide modern completion capabilities in Vim.
" Requires to be recompiled when it updates.
" C-family code intelligence is a bit more complicated to install.
" Plugin 'Valloric/YouCompleteMe'

Plugin 'epeli/slimux'

" Uses Ack as an improvement over grep searching.
Plugin 'mileszs/ack.vim'
Plugin 'rking/ag.vim'

" VCS (HG, Git) Plugins for Vim
Plugin 'tpope/vim-fugitive'
Plugin 'phleet/vim-mercenary'
Plugin 'vim-scripts/Lawrencium'



""""""""""""""""""""""""""""""""""""""""""""""""
" Other plugins which add useful functionality "
""""""""""""""""""""""""""""""""""""""""""""""""

Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-session'

" Distraction free writing
" Use :Goyo or :Goyo 80,
" As well as Limelight [0.0, 1.0], Limelight!!
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'

" Ultisnips is a snippets plugin for Vim.
" Plugin 'ervandew/supertab'
" Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

" TagBar provides nice code-browsing / structure overview.
"  Needs exuberant ctags.
Plugin 'majutsushi/tagbar'

""""""""""""""""""""
" Language Plugins "
""""""""""""""""""""

" Elm-Lang
Plugin 'ElmCast/elm-vim'

" Fish
Plugin 'dag/vim-fish'

" Plugins for improved language support
" Plugin 'sukima/xmledit'
" JSON
Plugin 'elzr/vim-json'

" nixpkgs nix syntax
Plugin 'spwhitt/vim-nix'

" Ruby
Plugin 'vim-ruby/vim-ruby'

" All of your Plugins must be added before the following line
call vundle#end()            " required by Vundle
filetype plugin indent on    " required by Vundle

syntax enable

set autoindent                     " Enables Auto Indent on files without type
set background=dark
set backspace=eol,start,indent     " Have backspace work properly.
set cmdheight=2                    " Height of Command bar.
set colorcolumn=80                 " highlight column to encourage short lines.
set copyindent
set encoding=utf-8
set expandtab
" Enable use of .exrc
" as per http://www.ilker.de/specific-vim-settings-per-project.html
set exrc
set foldlevelstart=5
set foldmethod=syntax
" Tips from http://blog.sanctum.geek.nz/vim-annoyances/
" Replace all occurrences during substititon (s/pat/repl/) by default; adding
" g will keep it to the first occurrence
set gdefault
set hidden                         " Lets Vim hide a buffer instead of closing.
                                   " so doesn't throw error about unsaved changes.
set hlsearch                       " Enable Search Highlighting
set incsearch                      " Enable search while typing
set ignorecase
set laststatus=2                   " for vim airline
set list listchars=tab:→\ ,trail:· " for highlighting whitespace
set mouse=a                        " use mouse
set nobackup                       " Rely on VCS instead
set noswapfile                     " Rely on VCS instead
set number                         " Enable Line Number
set ruler                          " Enable Ruler
set secure                         " for exrc
set shiftwidth=4                   " indentation
set showcmd                        " Shows cmd
set showmode                       " Shows mode
set showmatch                      " Show matching brackets
set smartcase                      " Be smart about cases when searching
set softtabstop=4                  " indentation
" Better defaults for new splits:
" set splitbelow
set splitright
" To help CtrlP ignore files
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.tar,*.o,*.hi,*.dyn_o
set wildignore+=*.class,*.jar      " JVM class files.
set wildignore+=*.pyc              " Python compiled *.pyc files.
set wildignore+=.git
set wildignore+=_site
set wildignore+=build
set wildignore+=target


" Custom Leader: <Space>
let mapleader=" "
let maplocalleader=" "
nnoremap <Space> <Nop>


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

" From http://vim.wikia.com/wiki/Remove_unwanted_spaces
" The variable _s is used to save and restore the last search pattern
" The e flag is used in the substitute command so no error is shown if
"  trailing whitespace is not found
nnoremap <silent> <F2> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Opposite of Shift-J,
" from http://vim.wikia.com/wiki/Insert_newline_without_entering_insert_mode
nnoremap <C-J> a<CR><Esc>k$

" Search for word under cursor
" Adapted from https://robots.thoughtbot.com/faster-grepping-in-vim
" bind K to grep word under cursor
nnoremap K :Ag! "\b<C-R><C-W>\b"<CR>:cw<CR>


" .md as Markdown
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.markdown set filetype=markdown



" Truecolor color scheme
colorscheme flattened_dark
" colorscheme flattened_light



" Jenkins/Jelly files are XML (in lieu of not having Jelly syntax)
au BufRead,BufNewFile *.jelly setfiletype xml



" Have YouCompleteMe and eclim
"  play nicely with each other.
let g:EclimCompletionMethod = 'omnifunc'



" https://vi.stackexchange.com/questions/9693/vim-omnicomplete-with-ruby-2-3-1
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby compiler ruby



" YouCompleteMe and Ultisnips,
" As per (last updated Jan 22 2014)
" http://stackoverflow.com/questions/14896327/ultisnips-and-youcompleteme

" make YCM compatible with UltiSnips (using supertab)
" As per supertab, use c-n, c-p to cycle autocomplete; use tab for Ultisnips
let g:ycm_key_list_select_completion   = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType    = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger       = "<tab>"
let g:UltiSnipsJumpForwardTrigger  = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"



" Customize YCM
let g:ycm_complete_in_comments                          = 1
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_collect_identifiers_from_tags_files           = 1



" Customize airline

let g:airline_powerline_fonts=1

" Shorter modeline map.
let g:airline_mode_map = {
    \ '__' : '-',
    \ 'n'  : 'N',
    \ 'i'  : 'I',
    \ 'R'  : 'R',
    \ 'c'  : 'C',
    \ 'v'  : 'V',
    \ 'V'  : 'V',
    \ '' : 'V',
    \ 's'  : 'S',
    \ 'S'  : 'S',
    \ '' : 'S',
    \ }

" Airline algo: tabs then spaces.
let g:airline#extensions#whitespace#mixed_indent_algo = 2

" Personally I love this idea,
" but unsuitable for large codebase you didn't write yourself.
let g:airline#extensions#whitespace#checks = []


" For the Sessions plugin
" Session autosaving is tedious.
let g:session_autosave = 'no'
let g:session_autoload = 'no'



" For slimux
" NOTE: Unfortunately, some OCaml plugin has mapping for
" something else (goto-interface).
map  <Leader>s :SlimuxREPLSendLine<CR>
vmap <Leader>s :SlimuxREPLSendSelection<CR>
map  <Leader>a :SlimuxShellLast<CR>
map  <Leader>k :SlimuxSendKeysLast<CR>



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
