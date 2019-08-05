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

;;; CODE:
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(defvar workstation-file "File location for a local Emacs Lisp configuration file")
(setq workstation-file "~/.emacs.d/local.el")
(when (file-exists-p workstation-file)
  (load workstation-file))

;; TODO: Consider the merits of each of these. (vs using Custom to do it).
;; from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(setq delete-old-versions -1)           ; delete excess backup versions silently
(setq version-control t)                ; use version control
(setq vc-make-backup-files t)           ; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t )            ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t)         ; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore)       ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8) ; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)    ; sentence SHOULD end with only a point.
(setq fill-column 80)           ; toggle wrapping text at the 80th character
;; (setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
(setq select-enable-clipboard nil)      ; use of killring / clipboard annoys me
(setq global-linum-mode nil)            ; don't enable line numbers. (IMO, too slow on large files with hidden areas)
(setq-default indent-tabs-mode nil)
(setq
 org-modules
 '(org-bbdb
   org-bibtex
   org-docview
   org-gnus
   org-habit
   org-info
   org-irc
   org-mhe
   org-rmail
   org-tempo
   org-w3m))
(setq vc-follow-symlinks t)
(setq
 custom-safe-themes
 '("d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347"
   "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f"))
(setq compilation-scroll-output 'first-error)

(setq ediff-split-window-function #'split-window-horizontally)

(add-hook 'text-mode-hook 'whitespace-mode)
(add-hook 'haskell-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'whitespace-mode)



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
(require 'subr-x)
(straight-use-package 'git)

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



(straight-use-package 'use-package)

(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-leader)
(straight-use-package 'evil-ledger)
(straight-use-package 'evil-magit)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-surround)
(straight-use-package
 '(evil-unimpaired :type git :host github :repo "zmaas/evil-unimpaired"))
(straight-use-package
 '(evil-org-mode :type git :host github :repo "Somelauw/evil-org-mode"))
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-surround)

(straight-use-package 'flycheck)
(straight-use-package 'flycheck-package)

;; general is a more generalised package compared to evil-leader
;; h/t https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(straight-use-package 'general)

(straight-use-package 'helm)
(straight-use-package 'ag)
(straight-use-package 'helm-ag)
(straight-use-package 'helm-rg)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-swoop)
(straight-use-package 'helm-themes)
(straight-use-package 'helm-descbinds)
(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)
(straight-use-package 'helm-org-rifle)

(straight-use-package 'hydra)

(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)

(straight-use-package 'magit)

(straight-use-package 'projectile)

(straight-use-package 'which-key)

(straight-use-package
 '(discover-my-major :type git :host github :repo "jguenther/discover-my-major"))

;; must run all-the-icons-install-fonts command
;; and (on windows) install manually
;; h/t: https://github.com/domtronn/all-the-icons.el
;; powershell: h/t:
;;   https://superuser.com/questions/201896/how-do-i-install-a-font-from-the-windows-command-prompt
;;  $fonts = (New-Object -ComObject Shell.Application).Namespace(0x14)
;;  Get-ChildItem -Recurse -include *.ttf | % { $fonts.CopyHere($_.fullname) }
(straight-use-package 'all-the-icons)
(straight-use-package 'doom-themes)
(straight-use-package 'doom-modeline)

(straight-use-package 'ace-link)
(straight-use-package 'avy)

(straight-use-package 'helpful)

(straight-use-package 'neotree)

(straight-use-package 'rainbow-delimiters)

(straight-use-package 'ranger)
(straight-use-package 'sunrise-commander)

(straight-use-package 'smooth-scrolling)

(straight-use-package 'solarized-theme)

;; https://company-mode.github.io/
;; https://github.com/company-mode/company-mode/wiki/Third-Party-Packages
(straight-use-package 'company)
(straight-use-package 'company-jedi)
(straight-use-package '(lsp-mode
                        :type git
                        :host github
                        :repo "emacs-lsp/lsp-mode"))
(straight-use-package 'company-lsp)
(straight-use-package 'company-quickhelp)

(straight-use-package 'ht)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-haskell)

(straight-use-package 'robe)

(straight-use-package 'writeroom-mode)

(straight-use-package 'restclient)

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(straight-use-package 'realgud)
(straight-use-package '(realgud-node-inspect :type git :host github :repo "realgud/realgud-node-inspect"))
(straight-use-package '(realgud-trepan-ni :type git :host github :repo "realgud/realgud-trepan-ni"))

(straight-use-package 'indium)

;; When running this, I encounter the error:
;;> Building haskell-mode...
;;> apply: haskell-font-lock.el:0:0: error: error:
;;  (Local variables entry is missing the suffix)
;; ^ The solution to this is to config git to use autoclrf is input
;;   (IIRC)
(straight-use-package 'haskell-mode)

(straight-use-package 'json-reformat)

(straight-use-package 'ledger-mode)

(straight-use-package 'markdown-mode)

(straight-use-package 'rust-mode)

(straight-use-package 'scala-mode)

(straight-use-package 'feature-mode)

(straight-use-package 'rainbow-mode)

(straight-use-package 'tldr)




;; note: the evil-collection warns that this should be set to nil
;;       before loading evil, evil-collection
;; h/t: https://github.com/emacs-evil/evil-collection/issues/60
(setq evil-want-keybinding nil)


(use-package evil
  :config
  (evil-mode 1))

(use-package evil-collection
  :config
  ;; h/t https://github.com/emacs-evil/evil-collection/issues/53
  (setq evil-collection-outline-bind-tab-p nil)
  (evil-collection-init
   '(dired
     ibuffer)))

;; (use-package evil-leader)
(use-package evil-unimpaired
  :config
  (evil-unimpaired-mode 1))

(use-package evil-snipe
  :config
  (evil-snipe-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-snipe
  :init
  (setq evil-snipe-spillover-scope 'buffer)
  :config
  (evil-snipe-mode +1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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
  (which-key-add-key-based-replacements
    "C-x 4" "other window prefix")
  (which-key-add-key-based-replacements
    "C-x 5" "frame prefix")
  (which-key-add-key-based-replacements
    "C-x 6" "two column prefix")
  (which-key-add-key-based-replacements
    "C-x 8" "symbols prefix")
  (which-key-add-key-based-replacements
    "C-x @" "event apply prefix")
  (which-key-add-key-based-replacements
    "C-x X" "edebug prefix")
  (which-key-add-key-based-replacements
    "C-x a" "abbrev prefix")
  (which-key-add-key-based-replacements
    "C-x a i" "inverse prefix")
  (which-key-add-key-based-replacements
    "C-x n" "narrow prefix")
  (which-key-add-key-based-replacements
    "C-x r" "register prefix")
  (which-key-mode))



;; add some commands to switch to particular themes
;; see also: https://www.brautaset.org/articles/2017/hydra-theme-switcher.html
;; ^^ this seems to be a much cleve
(defun my-load-theme-dark ()
  (interactive)
  (load-theme 'solarized-dark))
(defun my-load-theme-light ()
  (interactive)
  (load-theme 'solarized-light))

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)

(defun rgoulter/cheatsheet-rifle ()
  "A convenience command for running helm-org-rifle against
a cheatsheet file"
  (interactive)
  (when (file-exists-p cheatsheet-org-file)
    (let ((helm-autoresize-min-height 50)
          (helm-autoresize-max-height 50))
      (helm-org-rifle-files (list cheatsheet-org-file)))))

;; Taken from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; GitHub: https://github.com/noctuid/general.el
(use-package general
  :config
  (general-define-key "C-'" 'avy-goto-word-1)
  (general-define-key
   ;; replace default keybindings
   "C-s" 'swiper)             ; search for string in current buffer
   ;; "M-x" 'counsel-M-x        ; replace default M-x with ivy backend
  (general-define-key
   :prefix "C-c"
   ;; bind to simple key press
    "b"   'ivy-switch-buffer  ; change buffer, chose using ivy
    "/"   'counsel-git-grep   ; find string in git project
    ;; bind to double key press
    "f"   '(:ignore t :which-key "files")
    "ff"  'counsel-find-file  ; find file using ivy
    "fr"  'counsel-recentf    ; find recently edited files
    ;; "p"   '(:ignore t :which-key "project")
    "pf"  '(counsel-git :which-key "find file in git dir")        ; find file in git project
    "t"   '(:ignore t :which-key "load theme")
    "td" '(my-load-theme-dark :which-key "solarized dark")
    "tl" '(my-load-theme-light :which-key "solarized light"))
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

    ;; simple command
    "/"   'counsel-ag
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")

    ";"   'helm-M-x

    "cx" 'clipboard-kill-region
    "cc" 'clipboard-kill-ring-save
    "cv" 'clipboard-yank

    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "ar" 'ranger
    "ad" 'dired
    "at" 'tldr
    "am" 'woman

    "oc" 'rgoulter/cheatsheet-rifle))

(use-package discover-my-major
 :general
 ("C-h C-m" 'discover-my-major))



(use-package helpful
  :general
  ("C-h f" #'helpful-callable)
  ("C-h v" #'helpful-variable)
  ("C-h k" #'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  ("C-c C-d" #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h F" #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'.
  ("C-h C" #'helpful-command))



;; For Editing Language: Haskell
(use-package haskell-mode
 :init
 (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))



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



;; moving my org-mode code to a separate Emacs Lisp file.
;; This contains (use-package org ...) and other settings.
(defvar cheatsheet-org-file nil "path to a cheatsheet org file")
(let ((personal-settings "~/org/settings.el"))
 (when (file-exists-p personal-settings)
   (load-file personal-settings)))




;; from http://tuhdo.github.io/helm-intro.html
(use-package helm
  :init
  (setq helm-command-prefix-key "C-c h")
  (require 'helm-config)
  :general
  ("M-x" 'helm-M-x)
  ("C-x C-f" 'helm-find-files)
  (helm-map "<tab>" 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (helm-map "C-i"   'helm-execute-persistent-action) ; make TAB work in terminal
  (helm-map "C-z"   'helm-select-action) ; list actions using C-z
  :config
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
  (helm-mode 1))



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



;; Disable GUI elements for a cleaner UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)



(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package writeroom-mode)



(use-package projectile
  :defer nil  ;; I tried `:commands` but this still didn't help. :/
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile-find-file)
  (setq projectile-switch-project-action 'helm-projectile)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-globally-ignored-directories (cons "node_modules" projectile-globally-ignored-directories))
  :general
  (projectile-mode-map "C-c p" 'projectile-command-map))



(use-package neotree
  :general
  ([f8] 'neotree-toggle))



(use-package scala-mode)



(use-package ivy)
(use-package counsel)
(use-package swiper)

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'after-init-hook 'company-quickhelp-mode))

(use-package flycheck
  :init (global-flycheck-mode t)
  :config
  (flycheck-package-setup))

;; 2018-11-08: TODO:
;; Ohhh. e.g. a "Refiling Hydra" could be for the actions I do when refiling:
;; - REFINE (+ next header)
;; - REFILE ??
;; - tag "backlog", or other popular tags
;; - refile-to, and my popular places?

(require 'lsp)
;; in case you are using client which is available as part of lsp refer to the
;; table bellow for the clients that are distributed as part of lsp-mode.el
;; (require 'lsp-clients)
;; (add-hook 'programming-mode-hook 'lsp)
(require 'lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'haskell-mode-hook 'flycheck-mode)
;; (require 'lsp-haskell)
;; (add-hook 'haskell-mode-hook 'lsp)

(straight-use-package 'intero)
;; (add-hook 'haskell-mode-hook 'intero-mode)


;; h/t https://github.com/emacs-lsp/lsp-haskell/issues/31
;; (lsp-define-stdio-client lsp-haskell "haskell" #'lsp-haskell--get-root
;;        ;; '("hie" "--lsp" "-d" "-l" "/tmp/hie.log"))
;;        ;; '("hie" "--lsp" "-d" "-l" "/tmp/hie.log" "--vomit"))
;;        (funcall lsp-haskell-process-wrapper-function (lsp--haskell-hie-command)))
;; (setq lsp-haskell-process-args-hie '("-d" "-l" "c:/Users/MX15PRO-Richard/AppData/Local/Temp/hie.log"))
;; lsp-haskell-process-args-hie is a variable defined in ‘emacs.el’.
;; Its value is ("-d" "-l" "/tmp/hie.log")
;; (setq debug-on-error t)
(require 'tree-widget)



;; narrow to region is a useful command,
;; (unless you accidentally invoke it).
(put 'narrow-to-region 'disabled nil)




;; (straight-use-package
;;  '(ribbon-dummy-demo :local-repo "d:/github/buffer-ribbon.el"))
(add-to-list 'load-path "D:/github/buffer-ribbon.el")
(load-library "buffer-ribbon")
;; (require 'buffer-ribbon)
;; (load-file "d:/github/buffer-ribbon.el/buffer-ribbon.el")
;; (load-file "d:/github/buffer-ribbon.el/buffer-ribbon-tests.el")

;; shift left, right; zoom, unzoom
;; TODO: hydra-head to select the buffer!
(defhydra hydra-patch-grid (global-map "C-c r")
  "patch-grid"
  ("h" evil-window-left  "select left")
  ("j" evil-window-down  "select down")
  ("k" evil-window-up    "select up")
  ("l" evil-window-right "select right")
  
  ("H" buffer-ribbon/scroll-patch-grid-left "shift left")
  ("L" buffer-ribbon/scroll-patch-grid-right "shift right")

  ("J" buffer-ribbon/zoom-selected-window "zoom in" :exit t)
  ("K" buffer-ribbon/unzoom "zoom out")

  ("b" switch-to-buffer "switch buffer")

  ("o" other-window "cycle to other-window")

  ("gw"
   (buffer-ribbon/select-patch-grid-window 0 0)
   "jump to grid tile 0, 0")
  ("ge"
   (buffer-ribbon/select-patch-grid-window 1 0)
   "jump to grid tile 1, 0")
  ("gr"
   (buffer-ribbon/select-patch-grid-window 2 0)
   "jump to grid tile 2, 0")
  ("gs"
   (buffer-ribbon/select-patch-grid-window 0 1)
   "jump to grid tile 0, 1")
  ("gd"
   (buffer-ribbon/select-patch-grid-window 1 1)
   "jump to grid tile 1, 1")
  ("gf"
   (buffer-ribbon/select-patch-grid-window 2 1)
   "jump to grid tile 2, 1"))

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "r" 'hydra-patch-grid/body)
