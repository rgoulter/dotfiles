#!/usr/bin/env bash
# Verify Emacs Lisp files have balanced parentheses.
set -euo pipefail

for file in "$@"; do
  emacs --batch --eval "
    (with-temp-buffer
      (insert-file-contents \"${file}\")
      (goto-char (point-max))
      (emacs-lisp-mode)
      (condition-case err
          (progn (check-parens) (kill-emacs 0))
        (error
          (message \"Unbalanced parentheses in %s\" \"${file}\")
          (kill-emacs 1))))"
done
