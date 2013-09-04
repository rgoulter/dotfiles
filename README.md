# Dotfiles

Because folk keep their dotfiles in a repo and stuff. The trendy thing to do..

## My Vim Setup

Keeping my .vimrc here. Because I don't have a dotfiles repo.
Previously, like a noob I wasn't making use of the excellent Vundle. Now things can be cleaner.
I am a Computer Science student at the National University of Singapore,
and my VIM setup seems to be just whatever I think is cool, and isn't particularly
specific to any thing.

### Plugins and Configurations this Repo has:

####.vimrc file explanation
While I didn't out-right copy this .vimrc from other sources, (that is,
I wrote the lines myself), it's not that I know every line of this 
by heart.

Much of the rest of the .vimrc is adapted from other .vimrc files
I found via Google. In particular,
the [example .vimrc on VIM wikia](http://vim.wikia.com/wiki/Example_vimrc),
and [one of these "Perfect Vim RC" pages](http://amix.dk/vim/vimrc.html).
Unfortunately, my .vimrc isn't so well documented as these example rcs.
(I should probably work on that).

### Plugins:

[Vundle](https://github.com/gmarik/vundle), first off, as a modern way of managing Vim plugins.
This replaces Pathogen, which was a good improvement on just manual plugin management.

[YouCompleteMe](https://github.com/Valloric/YouCompleteMe) seems the modern thing to use instead of [AutoComplPop](http://www.vim.org/scripts/script.php?script_id=1879),
as well as for plugins like [NeoComplete](https://github.com/Shougo/neocomplete.vim), or [Supertab](https://github.com/ervandew/supertab).

[Tern_for_vim](https://github.com/marijnh/tern_for_vim) is a plugin for JavaScript intelligent code completion.

[Ultisnips](https://github.com/SirVer/ultisnips) is a snippets plugin. (Works with YouCompleteMe, I gather).

[TagBar](https://github.com/majutsushi/tagbar) is nice for code-browsing / code structure.

[FuzzyFinder](http://www.vim.org/scripts/script.php?script_id=1984) is a nice vim script for loading files using a substring of
the filename. I'm given the impression there are more modern alternatives
to this, such as [Command-T](https://github.com/wincent/Command-T), or [ctrlp](https://github.com/kien/ctrlp.vim).

[Powerline](https://github.com/Lokaltog/powerline) is a way to get a nice and pretty line at the bottom of Vim.
One lighter alternative to this would be [airline](https://github.com/bling/vim-airline).

There are excellent packages I think I shall investigate adding to my Vim setup, such as
[syntastic](https://github.com/scrooloose/syntastic) (for syntax checking, linting, etc.),
[NERDTree](https://github.com/scrooloose/nerdtree) I've heard is good; [NERDCommenter](https://github.com/scrooloose/nerdcommenter) also.
[Surround](https://github.com/tpope/vim-surround) and [delimitMate](https://github.com/Raimondi/delimitMate) also look good.
