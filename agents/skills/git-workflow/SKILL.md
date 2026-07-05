---
name: git-workflow
description: >
  Git and PR workflow for Richard's repositories: one logical change per commit,
  small focused pull requests, amend or squash fixups on unpublished branches.
  Use when committing, opening PRs, stacking changes, fixing review feedback,
  or the user mentions atomic commits, focused PRs, squash, amend, or avoiding
  "fix typo" / "address review" commit chains.
metadata:
  short-description: "Atomic commits and focused PRs"
---

# Git workflow (Richard's repos)

Apply these rules when working in repositories Richard owns or maintains.

## Commits

- **One logical change per commit** — each commit should stand alone as a
  coherent unit (buildable/sensible on its own when possible).
- **Do not** stack `"fix"`, `"address review"`, `"typo"`, or `"oops"` commits on
  a feature branch that is still private / not published.
- **Prefer `git commit --amend`** (or interactive rebase to squash) to fold fixups
  into the commit they belong to, while the branch has not been pushed or shared.
- After a branch is **published** (pushed, PR open, others may have pulled),
  avoid rewriting history unless Richard asks — use new commits or explicit
  restack/squash with his approval.

## Pull requests

- **Prefer several smaller, focused PRs** over one large PR when the work
  naturally splits (independent concerns, easier review, parallel merge).
- Each PR should have a clear scope and title; avoid unrelated drive-by changes.
- When a change set is already split, keep follow-ups in new PRs rather than
  expanding scope of an approved one.

## When unsure

- Default to **fewer, cleaner commits** and **narrower PR scope**.
- If a fix belongs to the previous commit and nothing is published yet, **amend**
  instead of adding a new commit.
