(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-linum-mode t)
 '(indent-tabs-mode nil)
 '(show-trailing-whitespace t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ibuffer mode
;;  this manages buffers like Dired manages directories.
;;
;; from: http://tuhdo.github.io/emacs-tutor.html
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; TODO: Consider the merits of each of these. (vs using Custom to do it).
;; from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
; (setq delete-old-versions -1 )		; delete excess backup versions silently
; (setq version-control t )		; use version control
; (setq vc-make-backup-files t )		; make backups file even when in version controlled dir
; (setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
; (setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
; (setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
; (setq ring-bell-function 'ignore )	; silent bell when you make a mistake
; (setq coding-system-for-read 'utf-8 )	; use utf-8 by default
; (setq coding-system-for-write 'utf-8 )
; (setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
; (setq default-fill-column 80)		; toggle wrapping text at the 80th character
; (setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup

;; Reference: https://stackoverflow.com/questions/14302171/ctrlu-in-emacs-when-using-evil-key-bindings
(setq evil-want-C-u-scroll t)

; (require 'package)
;   (push '("marmalade" . "https://marmalade-repo.org/packages/")
;         package-archives )
;   (push '("melpa" . "http://melpa.milkbox.net/packages/")
;         package-archives)
;   (push '("melpa-stable" . "http://stable.melpa.org/packages/")
;         package-archives)

; Taken from http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
; See also: http://www.emacswiki.org/emacs/ELPA
; (setq package-enable-at-startup nil)
; (package-initialize)

;; Bootstrap `req-package'
; (unless (package-installed-p 'use-package) ; unless it is already installed
;   (package-refresh-contents) ; updage packages archive
;   (package-install 'req-package)) ; and install the most recent version of use-package

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; straight.el and use-package integration:
(setq straight-use-package-by-default t)

; install packages
(straight-use-package 'use-package)
; (straight-use-package 'org)
(straight-use-package 'evil)
(straight-use-package 'helm)
; (straight-use-package 'helm-config)
(straight-use-package 'magit)
(straight-use-package 'smooth-scrolling)

(require 'subr-x)
(straight-use-package 'git)

(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'helm-org-rifle)

; use straight.el to install a newer version of org-mode
; h/t: https://github.com/raxod502/straight.el#installing-org-with-straightel
(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(straight-use-package 'org)

(straight-use-package 'ledger-mode)

(straight-use-package 'evil-collection)
(straight-use-package 'evil-magit)
(straight-use-package 'evil-ledger)

(straight-use-package 'hydra)

(straight-use-package 'helm-swoop)

(straight-use-package 'rainbow-delimiters)

(straight-use-package 'projectile)

(straight-use-package 'yasnippet)

; evil-nerd-commenter

(straight-use-package 'flycheck)

(straight-use-package 'solarized-theme)

(straight-use-package 'writeroom-mode)


(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

; evil-want-keybinding
(setq evil-want-keybinding nil)

;; `req-package` is different than `use-package`,
;; in that `req-package` also lets this emacs config declare which packages
;;  are dependencies of a required package.
; (require 'req-package)

; (req-package org)

(use-package evil
             :config (evil-mode 1))

;; https://github.com/sellout/emacs-color-theme-solarized
; 930-stars on GitHub
;; (req-package color-theme-solarized
;;   :require color-theme
;;   :config (progn
;;             ; (set-frame-parameter nil 'background-mode 'dark) ; for GUI-only. I guess this breaks in term.
;;             (load-theme 'solarized t)))

;; 2018-10-07: couldn't install on new Win10 Laptop
; (req-package evil-leader)

;; 2018-10-07: couldn't install on new Win10 Laptop
; (req-package evil-org)

;; Rainbow Delimiters
;; Colours are pretty. :-)
;; GitHub: https://github.com/istib/rainbow-blocks
;;
;; Usage-Notes:
;; - use M-x rainbow-delimiters-mode to toggle.
;; - EVIL: Can use as Ex command.
; (req-package rainbow-delimiters
;   ; Enable for most programming languages:
;   :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; Ivy/Counsel/Swiper work together.
;; Recommended by: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; GitHub: https://github.com/abo-abo/swiper

;; Ivy is comparable to Helm.
;; TODO: Look up (or try) Ivy vs Helm.
; (req-package ivy)

;; 2018-10-07: couldn't install on new Win10 Laptop
; (req-package counsel)

; (req-package swiper)

;; 2018-10-07: couldn't install on new Win10 Laptop
; (req-package avy)

; (req-package which-key)

;; Taken from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; GitHub: https://github.com/noctuid/general.el
;(req-package general
;  :require avy
;  :config
;  (general-define-key "C-'" 'avy-goto-word-1)
;  (general-define-key
;   ;; replace default keybindings
;   "C-s" 'swiper             ; search for string in current buffer
;   "M-x" 'counsel-M-x        ; replace default M-x with ivy backend
;   )
;  (general-define-key
;   :prefix "C-c"
;   ;; bind to simple key press
;    "b"	'ivy-switch-buffer  ; change buffer, chose using ivy
;    "/"   'counsel-git-grep   ; find string in git project
;    ;; bind to double key press
;    "f"   '(:ignore t :which-key "files")
;    "ff"  'counsel-find-file  ; find file using ivy
;    "fr"	'counsel-recentf    ; find recently edited files
;    "p"   '(:ignore t :which-key "project")
;    "pf"  '(counsel-git :which-key "find file in git dir")        ; find file in git project
;    )
;;  (general-define-key
;;   :states '(normal visual insert emacs)
;;   :prefix "SPC"
;;   :non-normal-prefix "C-SPC"
;;
;;    ;; simple command
;;    "'"   '(iterm-focus :which-key "iterm")
;;    "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
;;    "/"   'counsel-ag
;;    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
;;    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")
;;
;;    ;; Applications
;;    "a" '(:ignore t :which-key "Applications")
;;    "ar" 'ranger
;;    "ad" 'dired)
;  )

;; 2018-10-07: couldn't install on new Win10 Laptop
;; For Editing Language: Haskell
; (req-package haskell-mode
;  :init
;  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;; Tuarag Mode
;; For Editing Language: OCaml
; (req-package tuareg
;   :mode (("\\.ml[ily]?$" . tuareg-mode)
;          ("\\.topml$" . tuareg-mode)
;          ("\\.topscript$" . tuareg-mode)))

;; Merlin
;; OCaml completion
;; For Editing Language: OCaml
; (req-package merlin
;   :init (setq ; merlin-use-auto-complete-mode t
;               merlin-error-after-save nil)
;
;   :config (add-hook 'tuareg-mode-hook 'merlin-mode)
;           ; See http://emacs.stackexchange.com/questions/12084/how-to-get-merlin-mode-to-work-in-emacs
;           ;(setq merlin-command 'opam)
;
;   ;; For some reason, having :bind here throws this out of whack. ???
;   ; :bind (("C-c <up>" . merlin-type-enclosing-go-up)
;   ;        ("C-c <down>" . merlin-type-enclosing-go-down))
;   )

; QUESTION: Why is it (add-hook ... #'whatever-mode) vs (add-hook ... 'whatever-mode) ???

; (req-package-finish)



; Reload Emacs settings
; taken from http://www.saltycrane.com/blog/2007/07/how-to-reload-your-emacs-file-while/
; (defun reload-dotemacs-file ()
;     "reload your .emacs file without restarting Emacs"
;     (interactive)
;     (load-file "~/.emacs"))



; Set to dark or light
; - QUESTION: These won't work if use `M-x customize-variable frame-background-mode`.
; - QUESTION: What's an idiomatic way of enabling something like this?
(defun solarized-light ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'light)
  (enable-theme 'solarized))
(defun solarized-dark ()
  (interactive)
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized))



; Disable the toolbar;
; can re-enable with `M-x tool-bar-mode`
(tool-bar-mode -1)



(setq org-agenda-files "~/org/agenda")

; from: https://orgmode.org/manual/Activation.html#Activation
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

; from: http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
; use C-x r j (jump-to-register)
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?o (cons 'file "~/org/capture.org"))

(setq org-refile-targets '((org-agenda-files . (:tag . "refile"))))

(setq org-default-notes-file "~/org/capture.org")

(setq org-todo-keywords
      '((sequence "REFILE(f)" "REFINE(r)" "TODO(d)" "|" "DONE(D)")
        (sequence "WTB(w)" "TBR(b)" "TOUCHED(r)" "|" "READ(R)")))
; Additionally, want:
; - "journal/log": to-process | processed
;   (b/c sometimes when logging I'll have thoughts,
;    but I want it to be part of the log, not to capture it separately)
; - issues: to-address | solution-listed fixed


; https://orgmode.org/manual/Storing-searches.html#Storing-searches
; (setq org-agenda-custom-commands
;       '(("x" agenda)
;         ("y" agenda*)
;         ("w" todo "WAITING")
;         ("W" todo-tree "WAITING")
;         ("u" tags "+boss-urgent")
;         ("v" tags-todo "+boss-urgent")
;         ("U" tags-tree "+boss-urgent")
;         ("f" occur-tree "\\<FIXME\\>")
;         ("o" "Agenda and Office-related tasks"
;          ((agenda "")
;           (tags-todo "work")
;           (tags "office")))
;         ("h" . "HOME+Name tags searches") ; description for "h" prefix
;         ("hl" tags "+home+Lisa")
;         ("hp" tags "+home+Peter")
;         ("hk" tags "+home+Kim")))
(setq org-agenda-custom-commands
      '(("r" . "Refile Tasks (excl. backlog)")
        ("r" tags "TODO=\"REFILE\"-backlog")
        ("R" tags-tree "TODO=\"REFILE\"-backlog")
        ("i" . "Refine Tasks (excl. backlog)")
        ("i" tags "TODO=\"REFINE\"-backlog")
        ("I" tags-tree "TODO=\"REFINE\"-backlog")
        ("i" . "Pick Up and Do")
        ("p" tags "TODO=\"TODO\"+refile-backlog")
        ("P" tags-tree "TODO=\"TODO\"+refile-backlog")
        ("o" "Meta"
         ((tags "TODO=\"REFILE\"-backlog")
          (tags "TODO=\"REFINE\"-backlog")
          (tags-todo "refile-backlog/TODO")
          (tags-todo "backlog")))
        ))

; ido, is easier to get started with than helm
; (ido-mode)
; (setq org-completion-use-ido t)

; Apparently needed for emacs-org-mode code to look pretty
; https://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
(setq org-src-fontify-natively t)


; from http://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

; via https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling


; magit
(global-set-key (kbd "C-x g") 'magit-status)

; org-mode: I want RET to indent
(require 'org)
(define-key org-mode-map (kbd "\C-m")     'org-return-indent)

; This seems to speed-up Emacs when using "unicode characters"
; h/t https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(setq inhibit-compacting-font-caches t)
