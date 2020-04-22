# Use vim as our editor
# (Can set to nvim in config.user.fish)
set -x EDITOR vim

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

# json.tool validates and pretty-prints JSON
# https://unix.stackexchange.com/questions/283025/cant-find-documentation-on-json-tool
alias pyjson="python -m json.tool"
# simple http static server with Python
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


source $HOME/.config/fish/coloured-manpages.fish

# Legacy Keybindings for FZF conflict with new Fish keybindings in fish 2.4
set -U FZF_LEGACY_KEYBINDINGS 0

# Use local/user config.fish if it exists
if test -f $HOME/.config/fish/config.user.fish
    source $HOME/.config/fish/config.user.fish
end
