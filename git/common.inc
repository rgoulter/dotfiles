[alias]
	fancyoneline = log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit

	# adapted from https://gggritso.com/human-git-aliases
	branches = branch -a
	tags = tag
	stashes = stash list
	remotes = remote -v

	cleanslate = !git reset --hard HEAD && git clean -d -f

	unmerged = diff --name-only --diff-filter=U
	staged = diff --cached --diff-algorithm=minimal -w

	unstage = reset -q HEAD --
	discard = checkout --
	uncommit = reset --mixed HEAD~
	amend = commit --amend
[init]
	defaultBranch = "master"
[merge]
	conflictStyle = diff3
