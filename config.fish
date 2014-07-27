# Set Application/MacVim's Vim as our EDITOR
set -x EDITOR /Applications/MacVim.app/Contents/MacOS/Vim

set -x CLICOLOR 1
# set -x LSCOLORS ExFxCxDxBxegedabagacad

set -x myfoo mybar123

# From https://github.com/ndbroadbent/scm_breeze 
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit" 

set -x SUNF rgoulter@sunfire.comp.nus.edu.sg
set -x PATH /usr/local/bin $PATH

# OSX Bundles are messed up, so we need to alias like this.
alias vim='/Applications/MacVim.App/Contents/MacOS/Vim'

# Haskell binaries
set -x PATH $HOME/Library/Haskell/bin $PATH
set -x PATH $HOME/Library/Python/2.7/bin $PATH

# For Python / OpenCV, as installed by HomeBrew
set -x PYTHONPATH /usr/local/lib/python2.7/site-packages:$PYTHONPATH

set -x GOPATH ~/golang/packages
set -x GOROOT /usr/local/go
set -x PATH $GOROOT/bin $PATH

# Set FISH prompt to use Powerline, 'cause why not, right?
set fish_function_path $fish_function_path "/Users/richardgoulter/github/powerline/powerline/bindings/fish"
powerline-setup
