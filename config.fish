# Set Application/MacVim's Vim as our EDITOR
# set -x EDITOR /Applications/MacVim.app/Contents/MacOS/Vim

# set -x CLICOLOR 1
# set -x LSCOLORS ExFxCxDxBxegedabagacad

# set -x myfoo mybar123

# From https://github.com/ndbroadbent/scm_breeze 
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit" 

set -x SUNF rgoulter@sunfire.comp.nus.edu.sg
set -x AWSASSG1 ec2-user@ec2-54-254-239-146.ap-southeast-1.compute.amazonaws.com
set -x LORIS82 richardg@loris-82.ddns.comp.nus.edu.sg
set -x LORIS88 richardg@loris-88.ddns.comp.nus.edu.sg
# set -x PATH /usr/local/bin $PATH

# OSX Bundles are messed up, so we need to alias like this.
# alias vim='/Applications/MacVim.App/Contents/MacOS/Vim'

# Haskell binaries
# set -x PATH $HOME/Library/Haskell/bin $PATH
set -x PATH $HOME/.cabal/bin $PATH
# set -x PATH $HOME/Library/Python/2.7/bin $PATH
set -x PATH $HOME/.local/bin $PATH

set -x JAVA_HOME /usr/lib/jvm/java-default-runtime

# For Python / OpenCV, as installed by HomeBrew
# set -x PYTHONPATH /usr/local/lib/python2.7/site-packages:$PYTHONPATH

# set -x GOPATH ~/golang/packages
# set -x GOROOT /usr/local/go
# set -x PATH $GOROOT/bin $PATH

# Set FISH prompt to use Powerline, 'cause why not, right?
# /home/richard/.local/lib/python3.4/site-packages/powerline
set fish_function_path $fish_function_path "/home/richard/.local/lib/python3.4/site-packages/powerline/bindings/fish"
powerline-setup
