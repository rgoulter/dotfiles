---
name: git-workflow
description: >
  Git and PR workflow for Richard's repositories:
  organised atomic commits,
   small focused pull requests,
   amend or squash fixups
   instead of a new commit per change.
  Use when committing,
   opening PRs,
   stacking changes,
   fixing review feedback,
  or the user mentions atomic commits,
   focused PRs,
   squash,
   amend,
   or avoiding "fix typo" / "address review" commit chains.
metadata:
  short-description: "Atomic commits and focused PRs"
---

# Git workflow (Richard's repos)

## Commits

- **Organised, atomic commits** — one logical change each.
  Not one commit of everything, and not a new commit per tweak or review comment.
- **Amend or squash**
  `"fix"`, `"address review"`, `"typo"`, and `"oops"` into the commit they belong to.
  An open PR is still work in progress:
  prefer rewriting the branch (amend, squash, restack, force-push)
  over stacking fixup commits.

## Pull requests

- **Prefer several smaller, focused PRs**
  over one large PR when the work naturally splits.
- Clear scope and title; no unrelated drive-by changes.
  Don't expand an approved PR - open a follow-up.
