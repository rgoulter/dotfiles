# Use vim as our editor
# (Can set to nvim in config.user.fish)
set -x EDITOR vim

# This breaks 'help'
# set -x BROWSER /usr/bin/firefox

# HOST, for HG stuff.
set -x HOST (hostname)


# From https://github.com/ndbroadbent/scm_breeze
# See also: http://git-scm.com/docs/pretty-formats
# %h  - abbrev commit hash
# %d  - ref names (like `(origin/master)`)
# %s  - subject (first line of commit)
# %cr - committer date, relative.
# %an - author name.
alias gl="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"

# Set a template for `git init` to use.
# This is so I can have useful hooks in my projects.
#
# Can also be set in ~/.gitconfig like:
#  [init]
#    templatedir = ~/.git_template
set -x GIT_TEMPLATE_DIR ~/.git_template/

# HG, one-liner logs.
# from http://stackoverflow.com/questions/3575189/mercurial-log-with-one-liners
# See also: hgbook.red-bean.com/read/customizing-the-output-of-mercurial.html
#           https://www.selenic.com/hg/help/templates
# and http://stackoverflow.com/questions/3625725/can-i-add-custom-colors-to-mercurial-command-templates
# node|short - commit hash
# rev - int, revision number.
# branches/branch - branchname.
# desc|strip|firstline - 'subject'
# date|age - human-readable difference in time.
# author|person - Richard from Richard <richard.goulter@gmail.com>
# alias hl="hg log --template '{node|short} | {date|isodatesec} | {author|user}: {desc|strip|firstline}\n'"
# and hgrc like:
# [color]
# custom.rev = red
# custom.decorate = yellow
# custom.date = green
# custom.author = blue bold
alias hl="hg log --style ~/.hgrc.d/fancy.style"

# Git Clone from CliPboard.
alias gccp="git clone (xclip -out)"

# Because I forget the actual commands for these
alias pyjson="python -m json.tool"
alias py3serve="python3 -m http.server"

# In case memory is playing up?
# From http://www.linuxquestions.org/questions/linux-general-1/how-to-show-the-memory-usage-per-process-160181/
function mem_usage_of_processes
    ps -e -orss=,args= | sort -b -k1,1n | pr -TW$COLUMNS
end

# I've heard this is amusing
alias fucking=sudo

# Fish, sudo last command
# (Easier to write this way than using alias, where fish would eval var..).
function please
    eval sudo $history[1]
end


# Convenience variables so SSH'ing isn't tedious to type out
# (May make more sense to put this in .ssh/config?).
set -x SUNF rgoulter@sunfire.comp.nus.edu.sg

set -x LORIS5  richardg@loris-5.d2.comp.nus.edu.sg
set -x LORIS7 richardg@loris-7.ddns.comp.nus.edu.sg
set -x LORIS82 richardg@loris-82.ddns.comp.nus.edu.sg
set -x LORIS88 richardg@loris-88.ddns.comp.nus.edu.sg

alias l5="ssh $LORIS5"
alias l7="ssh $LORIS7"
alias l82="ssh $LORIS82"
alias l88="ssh $LORIS88"

set -x DIGOCEAN rgoulter@rgoulter.com


# ANTLR 4.4, Arch Linux
set ANTLR_PATH /usr/share/java/antlr-4.4-complete.jar
if not contains $ANTLR_PATH $CLASSPATH
    set -x CLASSPATH . $CLASSPATH
    set -x CLASSPATH $ANTLR_PATH $CLASSPATH
    alias grun='java org.antlr.v4.runtime.misc.TestRig'
end

# Haskell
# set -x PATH $HOME/.cabal/bin $PATH

# NPM
# set -x PATH ~/node_modules/.bin/ $PATH

# Elm-Env
#$ git clone --recurse-submodules https://github.com/sonnym/elmenv.git ~/.elmenv
# set -x PATH ~/.elmenv/bin/ $PATH

# Ruby
# set -x PATH $HOME/.gem/ruby/2.1.0/bin $PATH

# OCaml
# OPAM configuration
if test -d $HOME/.opam
    # source $HOME/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true
end

source $HOME/.config/fish/coloured-manpages.fish

# Use local/user config.fish if it exists
if test -f $HOME/.config/fish/config.user.fish
    source $HOME/.config/fish/config.user.fish
end
