;; This is my emacs ~/.emacs.d/init.el file.
;;
;; My current main use-case for emacs is the excellent org-mode.
;;
;; I try to make a note of the sources of inspiration
;; for various configuration improvements.
;; Main sources have been (in no particular order):
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html
;;   - https://github.com/emacs-tw/awesome-emacs
;;   - http://tuhdo.github.io/index.html
;;     - http://tuhdo.github.io/helm-intro.html
;;   - https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

;; NOTE: Comments use `;;` since this is the ELisp convention.
;; (And emacs will indent `;` to the right because of this!).
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html#Comment-Tips

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d737a2131d5ac01c0b2b944e0d2cb0be1c76496bb4ed61be51ff0e5457468974" "bf3ec301ea82ab546efb39c2fdd4412d1188c7382ff3bbadd74a8ecae4121678" default)))
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

;; TODO: Consider the merits of each of these. (vs using Custom to do it).
;; from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; (setq delete-old-versions -1 )		; delete excess backup versions silently
;; (setq version-control t )		; use version control
;; (setq vc-make-backup-files t )		; make backups file even when in version controlled dir
;; (setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
;; (setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
;; (setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
;; (setq ring-bell-function 'ignore )	; silent bell when you make a mistake
;; (setq coding-system-for-read 'utf-8 )	; use utf-8 by default
;; (setq coding-system-for-write 'utf-8 )
;; (setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
;; (setq default-fill-column 80)		; toggle wrapping text at the 80th character
;; (setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup



;; bootstrap straight.el
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

;; straight.el and use-package integration:
;; (setq straight-use-package-by-default t)

;; 2018-10-31: I ran into a problem when refiling, which I hadn't earlier.
;; kludge from raxod502/straight.el readme for installing org
;; h/t: https://github.com/raxod502/straight.el#installing-org-with-straightel
;; (require 'subr-x)
;; (straight-use-package 'git)
;;
;; ; use straight.el to install a newer version of org-mode
;; ; h/t: https://github.com/raxod502/straight.el#installing-org-with-straightel
;; (defun org-git-version ()
;;   "The Git version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (git-run "describe"
;;               "--match=release\*"
;;               "--abbrev=6"
;;               "HEAD"))))
;;
;; (defun org-release ()
;;   "The release version of org-mode.
;; Inserted by installing org-mode or when a release is made."
;;   (require 'git)
;;   (let ((git-repo (expand-file-name
;;                    "straight/repos/org/" user-emacs-directory)))
;;     (string-trim
;;      (string-remove-prefix
;;       "release_"
;;       (git-run "describe"
;;                "--match=release\*"
;;                "--abbrev=0"
;;                "HEAD")))))
;;
;; (provide 'org-version)
;;
;; (straight-use-package 'org)



(straight-use-package 'use-package)

(straight-use-package 'evil)

(straight-use-package 'helm)

(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)

(straight-use-package 'magit)

(straight-use-package 'smooth-scrolling)

(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'helm-org-rifle)

(straight-use-package 'ledger-mode)

(straight-use-package 'evil-leader)

(straight-use-package 'evil-collection)

(straight-use-package 'evil-magit)

(straight-use-package 'evil-ledger)

(straight-use-package
 '(evil-unimpaired :type git :host github :repo "zmaas/evil-unimpaired"))

(straight-use-package 'hydra)

(straight-use-package 'helm-ag)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-swoop)

(straight-use-package 'rainbow-delimiters)

(straight-use-package 'projectile)

(straight-use-package 'ranger)
(straight-use-package 'sunrise-commander)

(straight-use-package 'hydra)

(straight-use-package 'yasnippet)

;; evil-nerd-commenter

(straight-use-package 'flycheck)

(straight-use-package 'solarized-theme)

(straight-use-package 'markdown-mode)

(straight-use-package 'writeroom-mode)

(straight-use-package 'neotree)

;; general is a more generalised package compared to evil-leader
;; h/t https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(straight-use-package 'avy)
(straight-use-package 'general)

(straight-use-package 'which-key)

(straight-use-package 'scala-mode)

;; https://company-mode.github.io/
;; https://github.com/company-mode/company-mode/wiki/Third-Party-Packages
(straight-use-package 'company)

;; When running this, I encounter the error:
;;> Building haskell-mode...
;;> apply: haskell-font-lock.el:0:0: error: error: (Local variables entry is missing the suffix)
;; (straight-use-package 'haskell-mode)






;; note: the evil-collection warns that this should be set to nil
;;       before loading evil, evil-collection
;; h/t: https://github.com/emacs-evil/evil-collection/issues/60
(setq evil-want-keybinding nil)


(use-package evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :config
  (evil-collection-init
     'neotree))

;; (use-package evil-leader)

;;; Rainbow Delimiters
;;
;; Colours are pretty. :-)
;; GitHub: https://github.com/istib/rainbow-blocks
;;
;; Usage:
;; - command `rainbow-delimiters-mode` to toggle.
(use-package rainbow-delimiters
  ; Enable for most programming languages:
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

;; Alternative to helm: Ivy/Counsel/Swiper work together.
;; h/t: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

(use-package which-key
  :config
  (which-key-mode))

;; Taken from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; GitHub: https://github.com/noctuid/general.el
(use-package general
  :config
  (general-define-key "C-'" 'avy-goto-word-1)
  (general-define-key
   ;; replace default keybindings
   "C-s" 'swiper             ; search for string in current buffer
   ;; "M-x" 'counsel-M-x        ; replace default M-x with ivy backend
   )
  (general-define-key
   :prefix "C-c"
   ;; bind to simple key press
    "b"	'ivy-switch-buffer  ; change buffer, chose using ivy
    "/"   'counsel-git-grep   ; find string in git project
    ;; bind to double key press
    "f"   '(:ignore t :which-key "files")
    "ff"  'counsel-find-file  ; find file using ivy
    "fr"  'counsel-recentf    ; find recently edited files
    ;; "p"   '(:ignore t :which-key "project")
    "pf"  '(counsel-git :which-key "find file in git dir")        ; find file in git project
    )
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; simple command
    "/"   'counsel-ag
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")

    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "ar" 'ranger
    "ad" 'dired)
  )

;; For Editing Language: Haskell
;; (use-package haskell-mode
;;  :init
;;  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;; Tuarag Mode
;; For Editing Language: OCaml
;; (use-package tuareg
;;   :mode (("\\.ml[ily]?$" . tuareg-mode)
;;          ("\\.topml$" . tuareg-mode)
;;          ("\\.topscript$" . tuareg-mode)))

;; Merlin
;; OCaml completion
;; For Editing Language: OCaml
;; (use-package merlin
;;   :init (setq ; merlin-use-auto-complete-mode t
;;               merlin-error-after-save nil)
;;
;;   :config (add-hook 'tuareg-mode-hook 'merlin-mode)
;;           ; See http://emacs.stackexchange.com/questions/12084/how-to-get-merlin-mode-to-work-in-emacs
;;           ;(setq merlin-command 'opam)
;;
;;   ;; For some reason, having :bind here throws this out of whack. ???
;;   ; :bind (("C-c <up>" . merlin-type-enclosing-go-up)
;;   ;        ("C-c <down>" . merlin-type-enclosing-go-down))
;;   )



;; Reload Emacs settings
;; taken from http://www.saltycrane.com/blog/2007/07/how-to-reload-your-emacs-file-while/
;; (defun reload-dotemacs-file ()
;;     "reload your .emacs file without restarting Emacs"
;;     (interactive)
;;     (load-file "~/.emacs"))



(setq org-agenda-files "~/org/agenda")

;; from: http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
;; use C-x r j (jump-to-register)
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?o (cons 'file "~/org/capture.org"))

(setq org-refile-targets '((org-agenda-files . (:tag . "refile"))
                           (nil . (:tag . "refile"))))

(setq org-default-notes-file "~/org/capture.org")



(setq org-todo-keywords
      '((sequence "REFILE(f)" "REFINE(r)" "TODO(d)" "|" "DONE(D)")
        (sequence "WTB(w)" "TBR(b)" "TOUCHED(r)" "|" "READ(R)")
        (sequence "|" "FAILED(F)")
        (sequence "EXTRACT(x)" "|" "PROCESSED(P)")
        (type "ISSUE(i)" "INVESTIGATE(q)" "|" "NOTED(N)")))

;; https://orgmode.org/manual/Storing-searches.html#Storing-searches
;; (setq org-agenda-custom-commands
;;       '(("x" agenda)
;;         ("y" agenda*)
;;         ("w" todo "WAITING")
;;         ("W" todo-tree "WAITING")
;;         ("u" tags "+boss-urgent")
;;         ("v" tags-todo "+boss-urgent")
;;         ("U" tags-tree "+boss-urgent")
;;         ("f" occur-tree "\\<FIXME\\>")
;;         ("o" "Agenda and Office-related tasks"
;;          ((agenda "")
;;           (tags-todo "work")
;;           (tags "office")))
;;         ("h" . "HOME+Name tags searches") ; description for "h" prefix
;;         ("hl" tags "+home+Lisa")
;;         ("hp" tags "+home+Peter")
;;         ("hk" tags "+home+Kim")))
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



;; from: https://orgmode.org/manual/Activation.html#Activation
(general-def "\C-cl" 'org-store-link)
(general-def "\C-ca" 'org-agenda)
(general-def "\C-cc" 'org-capture)
(general-def "\C-cb" 'org-switchb)

;; Apparently needed for emacs-org-mode SRC code blocks to look pretty
;; https://orgmode.org/worg/org-contrib/babel/examples/fontify-src-code-blocks.html
(setq org-src-fontify-natively t)

;; via https://www.reddit.com/r/emacs/comments/4366f9/how_do_orgrefiletargets_work/
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling


(setq org-agenda-show-outline-path t)

;; 2018-11-09:
;;   %b adds 'breadcrumbs' to the prefix
;;    of each entry in the agenda view.
;;    This may allow for using terse entry headings,
;;    but I'd have to consider the structure of my
;;    org agenda.
;; (setq org-agenda-prefix-format
;;       '((agenda . " %i %-12:c%?-12t% s%b")
;;         (todo . " %i %-12:c%b")
;;         (tags . " %i %-12:c%b")
;;         (search . " %i %-12:c%b")))

;; 2018-11-09: Will see if I prefer using org-mode with markup chars hidden.
;; h/t https://stackoverflow.com/questions/10969617/hiding-markup-elements-in-org-mode
(setq org-hide-emphasis-markers t)

;; c.f. parameters https://orgmode.org/manual/The-clock-table.html
(setq org-agenda-clockreport-parameter-plist
      '(:link      t
        :maxlevel  2
        ;; I like to use a large list of agenda files; showing 0:00 is noise.
        :fileskip0 t))

(setq org-capture-templates
      ;; basic capture, tries to imitate the default capture template.
      '(("c" "basic capture" entry (file "~/org/capture.org")
         ;; %? :: puts the cursor there after capture
         ;; %u :: inactive timestamp
         "* %?\n  %u\n"
         :clock-resume t)
        ("C" "basic capture (with context)" entry (file "~/org/capture.org")
         ;; %? :: puts the cursor there after capture
         ;; %u :: inactive timestamp
         ;; %a :: 'annotation'. links to context where the capture was made.
         "* %?\n  %u\n  %a\n"
         :clock-resume t)))

;; org-mode: I want RET to indent
(require 'org)
(general-def org-mode-map "\C-m" 'org-return-indent)


;; enable org babel evaluation for more than just emacs lisp
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)                    ;; bonus: can run C, C++, D-lang
   (R . t)
   (awk . t)
   (calc . t)
   (clojure . t)
   (emacs-lisp . t)
   (haskell . nil)
   (js . t)
   (ledger . t)
   (python . t)
   (ruby . t)
   (sqlite . t)))

(setq org-confirm-babel-evaluate nil)



;; Trade-off: this slows down the helm-org-rifle search,
;; but this better suits how I'd like to use the rifle.
;;
;; (idea: if the rifle is too slow, can let/disable this
;;  for a quick-rifle).
(setq helm-org-rifle-test-against-path t)



;; from http://tuhdo.github.io/helm-intro.html
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(general-def "C-c h" 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(general-def helm-map "<tab>" 'helm-execute-persistent-action) ; rebind tab to run persistent action
(general-def helm-map "C-i"   'helm-execute-persistent-action) ; make TAB work in terminal
(general-def helm-map "C-z"   'helm-select-action) ; list actions using C-z

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



;; magit
(general-def "C-x g" 'magit-status)

;; This seems to speed-up Emacs when using "unicode characters"
;; h/t https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(setq inhibit-compacting-font-caches t)


;; h/t https://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
(setq electric-indent-mode -1)

;; ibuffer mode
;;  this manages buffers like Dired manages directories.
;;
;; from: http://tuhdo.github.io/emacs-tutor.html
(general-def "C-x C-b" 'ibuffer)

(use-package solarized-theme
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-dark))

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)



;; Disable the toolbar.
;; NOTE: can re-enable with `M-x tool-bar-mode`
(tool-bar-mode -1)



(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package writeroom-mode)


(use-package projectile
;;  :bind
;;  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile-find-file)
  (setq projectile-switch-project-action 'helm-projectile))
(general-def projectile-mode-map "C-c p" 'projectile-command-map)

(general-def "M-x" 'helm-M-x)
(general-def "C-x C-f" 'helm-find-files)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)



(use-package neotree)
(general-def [f8] 'neotree-toggle)



(use-package scala-mode)

;; 2018-11-08: TODO:
;; Ohhh. e.g. a "Refiling Hydra" could be for the actions I do when refiling:
;; - REFINE (+ next header)
;; - REFILE ??
;; - tag "backlog", or other popular tags
;; - refile-to, and my popular places?
