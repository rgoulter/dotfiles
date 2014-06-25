# Set Application/MacVim's Vim as our EDITOR.
export EDITOR=/Applications/MacVim.app/Contents/MacOS/Vim

# Use FISH (Friendly Interactive SHell).
export SHELL=/usr/local/bin/fish

# Customize prompt to look cool
export PS1="\[\033[34m\]\u\[\033[0m\]@\h  \w$ "
#export PS1="\u@\h \d \t\w$ "

export CLICOLOR=1
export LSCOLORS=ExFxCxDxBxegedabagacad


# From https://github.com/ndbroadbent/scm_breeze
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
export SUNF=rgoulter@sunfire.comp.nus.edu.sg
export PATH=/usr/local/bin:$PATH

# OSX Bundles are messed up, so we need to alias like this.
alias vim='/Applications/MacVim.App/Contents/MacOS/Vim'

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi


# For other Haskell stuff?
export PATH="$HOME/Library/Haskell/bin:$PATH"
export PATH=$PATH:~/.cabal/bin

# For CMake, Qt 5
export CMAKE_PREFIX_PATH=/usr/local/Qt5.2.1/5.2.1/clang_64/:$CMAKE_PREFIX_PATH

# For Python / OpenCV, as installed by HomeBrew
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH



# Convenience env. variable for quick project cd.
export RACEPROJDIR=/Users/richardgoulter/Documents/cs3217/final-project-group-06/skeleton/RacerGame
