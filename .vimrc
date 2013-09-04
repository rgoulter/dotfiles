set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" Vundle is the way you want to handle Vim plugins.
Bundle 'gmarik/vundle'

" My Bundles here:
" YouCompleteMe replaces AutoComplPop, and other things,
"  to provide modern completion capabilities in Vim.
" Requires to be recompiled when it updates.
" C-family code intelligence is a bit more complicated to install.
Bundle 'Valloric/YouCompleteMe'
" Tern for Vim is for intelligent JavaScript code completion, etc.
Bundle 'marijnh/tern_for_vim'
" Ultisnips is a snippets plugin for Vim.
Bundle 'SirVer/ultisnips'
" TagBar provides nice code-browsing / structure overview.
"  Needs exuberant tags.
Bundle 'majutsushi/tagbar'
" Solarized Color Scheme.
Bundle 'altercation/vim-colors-solarized'
" Powerline is a pretty powerline to have.
" Requires fancy fonts to look super-pretty.
Bundle 'Lokaltog/powerline'
" For more convenient typing most at present.
Bundle 'Raimondi/delimitMate'


Bundle 'sukima/xmledit'
" vim-scripts repos
" L9 is needed for some things, I think.
Bundle 'L9'
" FuzzyFinder is a really cool way to open files,
"  so that you can open files with just a subsequence.
" A popular alternative (more modern?) is to use
" Command-T plugin.
Bundle 'FuzzyFinder'
 

" Required for Vundle, and many other things:
filetype plugin indent on

syntax enable
set background=dark
colorscheme solarized

set hlsearch    " Enable Search Highlighting
set incsearch   " Enable search while typing
set showmatch   " Show mating brackets
set ic          " Ignore Case
set smartcase   " Be smart about cases.

set cmdheight=2 " Height of Command bar.
set showmode    " Shows mode
set showcmd     " Shows cmd
set number      " Enable Line Number
set ruler       " Enable Ruler

"" Indentation Settings
set shiftwidth=4
set softtabstop=4
set expandtab
set autoindent  " Enables Auto Indent on files without type
filetype plugin on
filetype indent on

" Have backspace work properly.
set backspace=eol,start,indent

" Better command-line completion
set wildmenu

" Set to auto read when a file is changed from the outside
set autoread

" Don't need swapfiles. (I think).
set noswapfile

" Some nice convenient keys.
nmap <F3> :FufCoverageFile<CR>
nmap <F8> :TagbarToggle<CR> 

" So that we can use snippets..
let g:UltiSnipsExpandTrigger="<c-a>"
" might also want to adjust these. (Default c-j and c-k).
"g:UltiSnipsJumpForwardTrigger
"g:UltiSnipsJumpBackwardTrigger


" Have YouCompleteMe and eclim
"  play nicely with each other.
let g:EclimCompletionMethod = 'omnifunc'

" Powerline
"set rtp+=~/github/powerline/powerline/bindings/vim
"set laststatus=2 " Always display the statusline in all windows
"set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)


" For toggling menu + toolbar and stuff..
nnoremap <C-F1> :if &go=~#'m'<Bar>set go-=m<Bar>else<Bar>set go+=m<Bar>endif<CR>
nnoremap <C-F2> :if &go=~#'T'<Bar>set go-=T<Bar>else<Bar>set go+=T<Bar>endif<CR>
nnoremap <C-F3> :if &go=~#'r'<Bar>set go-=r<Bar>else<Bar>set go+=r<Bar>endif<CR>

" By default, hide menu + toolbar..
set go-=m
set go-=T
