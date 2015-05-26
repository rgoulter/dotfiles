# Use nvim as our editor
set -x EDITOR nvim


# From https://github.com/ndbroadbent/scm_breeze
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# In case memory is playing up?
# From http://www.linuxquestions.org/questions/linux-general-1/how-to-show-the-memory-usage-per-process-160181/
alias mem_usage_of_processes="ps -e -orss=,args= | sort -b -k1,1n | pr -TW80"

# I've heard this is amusing
alias fucking=sudo

# Fish, sudo last command
alias please="eval sudo $history[1]"


# Convenience variables so SSH'ing isn't tedious to type out
set -x SUNF rgoulter@sunfire.comp.nus.edu.sg
set -x LORIS82 richardg@loris-82.ddns.comp.nus.edu.sg
set -x LORIS88 richardg@loris-88.ddns.comp.nus.edu.sg

set -x DIGOCEAN rgoulter@rgoulter.com


# ANTLR 4.4, Arch Linux
set ANTLR_PATH /usr/share/java/antlr-4.4-complete.jar
if not contains $ANTLR_PATH $CLASSPATH
    set -x CLASSPATH . $CLASSPATH
    set -x CLASSPATH $ANTLR_PATH $CLASSPATH
    alias grun='java org.antlr.v4.runtime.misc.TestRig'
end

# Haskell
set -x PATH $HOME/.cabal/bin $PATH

# Ruby
set -x PATH $HOME/.gem/ruby/2.1.0/bin $PATH

# OCaml
# OPAM configuration
if test -d $HOME/.opam
    source $HOME/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true
end


# Use local/user config.fish if it exists
if test -f $HOME/.config/fish/config.user.fish
    source $HOME/.config/fish/config.user.fish
end
