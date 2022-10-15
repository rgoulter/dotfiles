#!/usr/bin/env bash

emacs \
  --batch \
  -u "$(whoami)" \
  --script ./emacs-straight-versions-bump.el
