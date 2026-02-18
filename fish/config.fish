set -x EDITOR hx

set -x SHELL (which fish)

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
#
# This is used by git-init
# https://git-scm.com/docs/git-init#_template_directory
# set -x GIT_TEMPLATE_DIR ~/.git_template/


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


if not set --query GOPATH
    set -x GOPATH "$HOME/go"
    set -x PATH $GOPATH/bin/ $PATH
end


source $HOME/.config/fish/coloured-manpages.fish


# Legacy Keybindings for FZF conflict with new Fish keybindings in fish 2.4
set -U FZF_LEGACY_KEYBINDINGS 0


# Use local/user config.fish if it exists
# (Source this at the end as an easy way of overriding variables set above).
if test -f $HOME/.config/fish/config.user.fish
    source $HOME/.config/fish/config.user.fish
end


direnv hook fish | source

if command -q starship; and test "$TERM" != "dumb"
    starship init fish | source
end

abbr -a nscf nix-shell --command "fish"
abbr -a nb nix-build

abbr -a cb cargo build
abbr -a ct cargo test

abbr -a mm make
abbr -a mc make clean
abbr -a mt make test

abbr -a j just

abbr -a we watchexec
