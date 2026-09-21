---
name: prose-style
description: >
  Dry technical prose for Richard's documentation and user-facing text:
  semantic line breaks, mixed sentence length, no aphorisms,
  no anthropomorphised objects.
  Use when writing or editing a README, Org file, docs, or other prose;
  when the user mentions LLM stench, staccato, aphorisms, semantic line breaks,
  dry or boring writing, or /prose-style.
  Commit messages follow git-workflow instead.
metadata:
  short-description: "Dry technical prose"
---

# Prose style

For documentation and other user-facing prose
 (README, Org, comments that explain).
Dry and boring is acceptable.
Commit messages follow **git-workflow**.

After a draft, re-read for stacked short sentences, pull-quote lines,
 objects that want, take, or write things,
 and sentences a reader who landed on this page does not need.

## Semantic line breaks

Start each sentence on its own line.
Wrap a long sentence at a clause boundary
 and indent each continuation by one space.

```
Org already provides TODO keywords, tags, properties, and agenda queries.
Interactive Emacs and `emacs --batch` edit the same files.
```

Tables and source blocks are unchanged.
A list item's first sentence starts after the bullet;
 a later sentence in that item aligns with the first;
 a wrap of the same sentence is one extra space.

## Sentence length

Mix short sentences with longer ones in a paragraph.
Join related facts instead of stacking five declarations of similar length.

## Explain

State what something is, or what to do.
Delete a sentence that would work as a slide caption
 (`These pages are the product.`, `Emacs is the extension point.`,
 `Each extra has a job.`, `Proof that batch is enough.`).
Drop a line that only restates the heading.

Tables may stay telegraphic.
Body prose should not.

## Subjects

The subject is a person, a program, or an instruction
 (`put`, `set`, `keep`, `omit`, `use`).
Directories, trees, stores, and files do not want, take, write, or care.

```
Put custom elisp in the working tree.
Omit archive/ from org-agenda-files.
```

## Audience

Write for the reader on this page, doing the task that page is for.
Include what they need to proceed.
Omit background, change history, authoring detail,
 and caveats about other viewers.
Those belong in commits, PRs, or maintainer docs.

After a draft, ask of each sentence whether a reader who landed here needs it.
If not, delete it.

## Contrast

Lead with the fact.
A “this is X, not Y” opening is usually filler;
 use a contrast only when the distinction is the content.

## Slash command

`/prose-style` — apply these rules to the current draft or edit.
