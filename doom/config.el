;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Richard Goulter"
      user-mail-address "richard.goulter@gmail.com")

;; M-SPC gets captured by Gnome
(setq doom-leader-alt-key "C-SPC")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
(setq doom-font (font-spec :family "Source Code Pro" :size 21))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(defvar workstation-file "File location for a local Emacs Lisp configuration file")
(setq workstation-file (concat "~/.config/doom" "local.el"))
(when (file-exists-p workstation-file)
  (load workstation-file))

;; moving my org-mode code to a separate Emacs Lisp file.
;; This contains (use-package org ...) and other settings.
(let ((personal-settings "~/org/settings.el"))
 (when (file-exists-p personal-settings)
   (load-file personal-settings)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Support USB HID keys
(general-define-key
 "<XF86Cut>" 'clipboard-kill-region
 "<XF86Copy>" 'clipboard-kill-ring-save
 "<XF86Paste>" 'clipboard-yank)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! blamer
  :general ("s-i" #'blamer-show-commit-info)
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(use-package! discover-my-major
 :config
 (map! :leader :n "h C-m" 'discover-my-major))

(after! epa
  ;; don't use minibuffer
  (setq epa-pinentry-mode 'ask))

(use-package! justl)

(use-package! ranger
  :config
  (map! :leader :desc "Ranger" :n "om" #'ranger))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

;; https://github.com/felipeochoa/rjsx-mode/issues/85
(add-hook 'rjsx-mode-hook (lambda () (setq-local indent-line-function 'js-jsx-indent-line)))

(use-package! zetteldeft
  :config
  ;; If creating more than one note in a minute
  ;; using zetteldeft, the default id generation
  ;; gives them the same ID.
  (setq zetteldeft-id-format "%Y-%m-%d-%H%M%S")
  :general
  (:prefix doom-leader-key
   :keymaps 'normal
   "d"  '(nil :wk "deft")
   "dd" '(deft :wk "deft")
   "dD" '(zetteldeft-deft-new-search :wk "new search")
   "dR" '(deft-refresh :wk "refresh")
   "ds" '(zetteldeft-search-at-point :wk "search at point")
   "dc" '(zetteldeft-search-current-id :wk "search current id")
   "df" '(zetteldeft-follow-link :wk "follow link")
   "dF" '(zetteldeft-avy-file-search-ace-window :wk "avy file other window")
   "dl" '(zetteldeft-avy-link-search :wk "avy link search")
   "dt" '(zetteldeft-avy-tag-search :wk "avy tag search")
   "dT" '(zetteldeft-tag-buffer :wk "tag list")
   "di" '(zetteldeft-find-file-id-insert :wk "insert id")
   "dI" '(zetteldeft-find-file-full-title-insert :wk "insert full title")
   "do" '(zetteldeft-find-file :wk "find file")
   "dn" '(zetteldeft-new-file :wk "new file")
   "dN" '(zetteldeft-new-file-and-link :wk "new file & link")
   "dr" '(zetteldeft-file-rename :wk "rename")))

;; Kludge: use gpg-agent as the ssh agent
(when (eq system-type 'gnu/linux)
  (setenv "SSH_AUTH_SOCK" (format "/run/user/%d/gnupg/S.gpg-agent.ssh" (user-uid))))
(when (eq system-type 'darwin)
  (setenv "SSH_AUTH_SOCK" (format "%s/.gnupg/S.gpg-agent.ssh" (getenv "HOME"))))
