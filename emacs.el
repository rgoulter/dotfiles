;;; init.el --- my emacs initialization file

;;; Commentary:
;;
;; This is my Emacs ~/.emacs.d/init.el file.
;;
;; Many parts of this configuration have been
;; inspired by (in no particular order):
;;   - http://pages.sachachua.com/.emacs.d/Sacha.html
;;   - https://github.com/emacs-tw/awesome-emacs
;;   - http://tuhdo.github.io/index.html
;;     - http://tuhdo.github.io/helm-intro.html
;;   - https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/

;; "~/.emacs.d/straight/repos/use-package/README.md"
;; "~/.emacs.d/straight/repos/general.el/README.org"

;;; CODE:

;;;;;; Set garbage collection threshold
;; h/t https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; From https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;;;;;; Set file-name-handler-alist
;; Also from https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(setq delete-old-versions -1)           ; delete excess backup versions silently
(setq version-control t)                ; use version control
(setq vc-make-backup-files t)           ; make backups file even when in version controlled dir
;; (setq backup-directory-alist `(("." . (concat user-emacs-directory "backups")))) ; which directory to put backups file
(setq backup-directory-alist `(("." . "~/.config/emacs-rgoulter/backups"))) ; which directory to put backups file
(setq vc-follow-symlinks t )            ; don't ask for confirmation when opening symlinked file
;; (setq auto-save-file-name-transforms '((".*" (concat user-emacs-directory "auto-save-list") t)) ) ;transform backups file name
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs-rgoulter/auto-save-list" t)) ) ;transform backups file name
(setq create-lockfiles nil)             ; For me, lockfiles can cause more problems than it solves
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)       ; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8)    ; use utf-8 by default
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)    ; sentence SHOULD end with only a point.
(setq fill-column 80)           ; toggle wrapping text at the 80th character
(setq select-enable-clipboard nil)      ; use of killring / clipboard annoys me
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
 '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358"
   "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3"))

;;; (custom-set-faces
;;;  ;; custom-set-faces was added by Custom.
;;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;;  ;; Your init file should contain only one such instance.
;;;  ;; If there is more than one, they won't work right.
;;;  '(whitespace-tab ((((class color) (min-colors 89)) (:background unspecified :foreground "#586e75" :inverse-video unspecified)))))

(setq compilation-scroll-output 'first-error)

(setq ediff-split-window-function #'split-window-horizontally)

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)

;; This seems to speed-up Emacs when using "unicode characters"
;; h/t https://emacs.stackexchange.com/questions/33510/unicode-txt-slowness
(setq inhibit-compacting-font-caches t)


;; h/t https://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
(setq electric-indent-mode -1)

;; Disable GUI elements for a cleaner UI
(progn
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(add-hook 'text-mode-hook 'whitespace-mode)
(add-hook 'haskell-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'whitespace-mode)


;; add some commands to switch to particular themes
;; see also: https://www.brautaset.org/articles/2017/hydra-theme-switcher.html
(defun my-load-theme-dark ()
  "A convenience command to load the solarized-dark theme."
  (interactive)
  (load-theme 'solarized-dark))


(defun my-load-theme-light ()
  "A convenience command to load the solarized-light theme."
  (interactive)
  (load-theme 'solarized-light))


(defun rgoulter/set-font-dyslexic-big ()
  "A convenience command for using the OpenDyslexic font."
    (interactive)
    (set-face-attribute 'default nil :font "OpenDyslexic 24" )
    (set-frame-font "OpenDyslexic 24" nil t))


(defun rgoulter/set-font-dyslexic-mono-big ()
  "A convenience command for using the OpenDyslexicMono font."
    (interactive)
    (set-face-attribute 'default nil :font "OpenDyslexicMono 20" )
    (set-frame-font "OpenDyslexicMono 20" nil t))


;; h/t https://www.reddit.com/r/emacs/comments/1xe7vr/check_if_font_is_available_before_setting/
;; Test char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;;
;; Probably not the best way of doing this.
(defun rgoulter/set-font-programming ()
  "A convenience command for setting a large, pretty font."
  (interactive)
  (dolist (font-name '("Inconsolata for Powerline" "Inconsolata Nerd Font"))
    (when (find-font (font-spec :name font-name))
      ;; e.g. "Inconsolata for Powerline 20"
      (let ((font-name-with-size (concat font-name " 18")))
        (set-face-attribute 'default nil :font font-name-with-size)
        (set-frame-font font-name-with-size nil t)))))


(defvar cheatsheet-org-file nil "Path to a cheatsheet org file.")
(defun rgoulter/cheatsheet-rifle ()
  "A convenience command for running helm-org-rifle against a cheatsheet file."
  (interactive)
  (when (file-exists-p cheatsheet-org-file)
    (let ((helm-autoresize-min-height 50)
          (helm-autoresize-max-height 50))
      (helm-org-rifle-files (list cheatsheet-org-file)))))


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

;; kludge from raxod502/straight.el readme for installing org
;; h/t: https://github.com/raxod502/straight.el#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

; use straight.el to install a newer version of org-mode
; h/t: https://github.com/raxod502/straight.el#installing-org-with-straightel
(defun org-git-version ()
  "The Git version of 'org-mode'.

Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of 'org-mode'.

Inserted by installing 'org-mode' or when a release is made."
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



;; Workaround macOS.
;; The /usr/local/bin (used by Homebrew, etc.) isn't on Emacs'
;; exec-path, etc., even though it's on the PATH
;; in the shell.
(straight-use-package 'exec-path-from-shell)

(straight-use-package 'pinentry)

(straight-use-package 'use-package)

(straight-use-package 'undo-tree)

(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-leader)
(straight-use-package 'evil-ledger)
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
(straight-use-package 'flycheck-projectile)

;; general is a more generalised package compared to evil-leader
;; h/t https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(straight-use-package 'general)

(straight-use-package 'ag)

(straight-use-package 'helm)
(straight-use-package 'helm-ag)
(straight-use-package 'helm-rg)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-swoop)
(straight-use-package 'helm-themes)
(straight-use-package 'helm-descbinds)
(straight-use-package 'helm-org-rifle)

(straight-use-package 'dash)
(straight-use-package 'f)
(straight-use-package 's)

(straight-use-package 'hydra)

;; Alternative to helm: Ivy/Counsel/Swiper work together.
;; h/t: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'swiper)

(straight-use-package 'magit)
(straight-use-package 'forge)

(straight-use-package 'github-review)

(straight-use-package 'projectile)

(straight-use-package 'which-key)

(straight-use-package 'org-jira)

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

(straight-use-package 'deft)

(straight-use-package '(zetteldeft
                        :type git
                        :host github
                        :repo "EFLS/zetteldeft"))

(straight-use-package 'diff-hl)

(straight-use-package 'dired+)

(straight-use-package 'helpful)

(straight-use-package 'neotree)

(straight-use-package 'rainbow-delimiters)

(straight-use-package 'ranger)
;; (straight-use-package 'sunrise-commander)

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

(straight-use-package 'docker)

(straight-use-package 'ht)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-haskell)

(straight-use-package 'robe)

(straight-use-package 'groovy-mode)

(straight-use-package 'elixir-mode)

(straight-use-package 'nix-mode)
(straight-use-package 'nix-sandbox)

(straight-use-package 'tide)

(straight-use-package 'writeroom-mode)

(straight-use-package 'restclient)

(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(straight-use-package 'realgud)
(straight-use-package '(realgud-node-inspect :type git :host github :repo "realgud/realgud-node-inspect"))
(straight-use-package '(realgud-trepan-ni :type git :host github :repo "realgud/realgud-trepan-ni"))

(straight-use-package 'indium)

(straight-use-package 'haskell-mode)

(straight-use-package 'elm-mode)

(straight-use-package 'json-reformat)

(straight-use-package 'ledger-mode)

(straight-use-package 'markdown-mode)

(straight-use-package 'go-mode)
(straight-use-package 'ob-go)

(straight-use-package 'plantuml-mode)

(straight-use-package 'rust-mode)

(straight-use-package 'scala-mode)

(straight-use-package 'terraform-mode)

(straight-use-package 'feature-mode)

(straight-use-package 'rainbow-mode)

(straight-use-package 'tldr)

(straight-use-package 'esup)

(straight-use-package 'kubernetes)
(straight-use-package 'kubernetes-evil)

(straight-use-package 'vterm)


(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(use-package evil-collection
  :init
  (setq evil-want-keybinding nil)
  :config
   ;; note: the evil-collection warns that this should be set to nil
   ;;       before loading evil, evil-collection
   ;; h/t: https://github.com/emacs-evil/evil-collection/issues/60
  ;; h/t https://github.com/emacs-evil/evil-collection/issues/53
  (setq evil-collection-outline-bind-tab-p nil)
  (evil-collection-init
   '(ag
     dired
     docker
     ibuffer
     magit))
  (evil-mode 1))

;; Hook unto-tree-mode onto evil-local-mode.
;;
;; For some reason, adding:
;;   :hook
;;   (evil-local-mode . undo-tree-mode)
;; to the use-package above doesn't work.
;; Nor:
;;   (use-package undo-tree
;;     :hook
;;     evil-local-mode)
(add-hook 'evil-local-mode-hook 'undo-tree-mode)


;; (use-package evil-leader)


(use-package evil-magit
  :hook
  (with-editor-mode . evil-insert-state))


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
  (add-hook
   'evil-org-mode-hook
   (lambda ()
     (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package evil-snipe
  :init
  (setq evil-snipe-spillover-scope 'buffer)
  :config
  (evil-snipe-mode 1))


(use-package evil-surround
  :config
  (global-evil-surround-mode 1))


(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))


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
  (which-key-mode 1))


;; Taken from: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
;; GitHub: https://github.com/noctuid/general.el
(use-package general
  :config
  (general-define-key
   ;; replace default keybindings
   "C-s" 'swiper)             ; search for string in current buffer
   ;; "M-x" 'counsel-M-x        ; replace default M-x with ivy backend
  ;; ibuffer mode
  ;;  this manages buffers in a similar manner to how Dired manages directories.
  ;;
  ;; from: http://tuhdo.github.io/emacs-tutor.html
  (general-def "C-x C-b" 'ibuffer)
  (general-define-key
   :prefix "C-c"
   ;; bind to simple key press
    "b"   'ivy-switch-buffer  ; change buffer, chose using ivy
    "/"   'counsel-git-grep   ; find string in git project
    ;; bind to double key press
    "f"   '(:ignore t :which-key "files")
    "ff"  'counsel-find-file  ; find file using ivy
    "fr"  'counsel-recentf    ; find recently edited files
    "t"   '(:ignore t :which-key "load theme")
    "td" '(my-load-theme-dark :which-key "solarized dark")
    "tl" '(my-load-theme-light :which-key "solarized light"))
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :keymaps 'override
   :non-normal-prefix "C-SPC"

    ;; simple command
    "/"   'counsel-ag
    "TAB" '(evil-switch-to-windows-last-buffer :which-key "prev buffer")
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")

    ";"   'helm-M-x

    "cx" 'clipboard-kill-region
    "cc" 'clipboard-kill-ring-save
    "cv" 'clipboard-yank

    "g" 'magit-status

    "p" 'projectile-command-map

    "A" '(lambda () (interactive) (org-agenda nil "a"))

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
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'.
  ;; Helpful already links to the manual, if a function is referenced there.
  ("C-h F" #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'.
  ("C-h C" #'helpful-command))


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


(use-package magit
  :general
  ("C-x g" 'magit-status))


(use-package forge
  :after magit)


(use-package solarized-theme
  :init
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0))


(use-package smooth-scrolling
 :config
 (smooth-scrolling-mode 1))


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


;; Alternative to helm: Ivy/Counsel/Swiper work together.
;; h/t: https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/
(use-package ivy)


(use-package counsel)


(use-package swiper)


(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init))


(use-package company
  :hook
  (after-init . global-company-mode)
  (after-init . company-quickhelp-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 3)
  (setq company-quickhelp-delay 0.1))


(use-package flycheck
  :init (global-flycheck-mode t)
  :config
  (flycheck-package-setup))


(use-package deft
  :init
  (setq deft-extensions '("org" "md" "txt")))


(use-package zetteldeft
  :after deft
  :config
  ;; If creating more than one note in a minute
  ;; using zetteldeft, the default id generation
  ;; gives them the same ID.
  (setq zetteldeft-id-format "%Y-%m-%d-%H%M%S")
  :general
  (:prefix "SPC"
   :non-normal-prefix "C-SPC"
   :states '(normal visual motion emacs)
   :keymaps 'override
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


(use-package avy
  :general
  ("C-'" 'avy-goto-word-1))


(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)


(use-package lsp-ui :commands lsp-ui-mode)


(use-package company-lsp :commands company-lsp)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'haskell-mode-hook 'flycheck-mode)


(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))


(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))


(use-package haskell-mode
 :hook
 (haskell-mode . turn-on-haskell-indent))


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . 'gfm-mode)
         ("\\.md\\'" . 'markdown-mode)
         ("\\.markdown\\'" . 'markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package nix-mode
  :mode "\\.nix\\'")


(use-package scala-mode)


;; (straight-use-package
;;  '(ribbon-dummy-demo :local-repo "d:/github/buffer-ribbon.el"))
;; (add-to-list 'load-path "D:/github/buffer-ribbon.el")
;; (load-library "buffer-ribbon")
;; (require 'buffer-ribbon)
;; (load-file "d:/github/buffer-ribbon.el/buffer-ribbon.el")
;; (load-file "d:/github/buffer-ribbon.el/buffer-ribbon-tests.el")

;; shift left, right; zoom, unzoom
;; TODO: hydra-head to select the buffer!
;; (defhydra hydra-patch-grid (global-map "C-c r")
;;   "patch-grid"
;;   ("h" evil-window-left  "select left")
;;   ("j" evil-window-down  "select down")
;;   ("k" evil-window-up    "select up")
;;   ("l" evil-window-right "select right")
;;   
;;   ("H" buffer-ribbon/scroll-patch-grid-left "shift left")
;;   ("L" buffer-ribbon/scroll-patch-grid-right "shift right")
;; 
;;   ("J" buffer-ribbon/zoom-selected-window "zoom in" :exit t)
;;   ("K" buffer-ribbon/unzoom "zoom out")
;; 
;;   ("b" switch-to-buffer "switch buffer")
;; 
;;   ("o" other-window "cycle to other-window")
;; 
;;   ("gw"
;;    (buffer-ribbon/select-patch-grid-window 0 0)
;;    "jump to grid tile 0, 0")
;;   ("ge"
;;    (buffer-ribbon/select-patch-grid-window 1 0)
;;    "jump to grid tile 1, 0")
;;   ("gr"
;;    (buffer-ribbon/select-patch-grid-window 2 0)
;;    "jump to grid tile 2, 0")
;;   ("gs"
;;    (buffer-ribbon/select-patch-grid-window 0 1)
;;    "jump to grid tile 0, 1")
;;   ("gd"
;;    (buffer-ribbon/select-patch-grid-window 1 1)
;;    "jump to grid tile 1, 1")
;;   ("gf"
;;    (buffer-ribbon/select-patch-grid-window 2 1)
;;    "jump to grid tile 2, 1"))
;; 
;; (general-define-key
;;  :states '(normal visual)
;;  :prefix "SPC"
;;  "r" 'hydra-patch-grid/body)


(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar workstation-file "File location for a local Emacs Lisp configuration file")
(setq workstation-file (concat user-emacs-directory "local.el"))
(when (file-exists-p workstation-file)
  (load workstation-file))

;; moving my org-mode code to a separate Emacs Lisp file.
;; This contains (use-package org ...) and other settings.
(let ((personal-settings "~/org/settings.el"))
 (when (file-exists-p personal-settings)
   (load-file personal-settings)))

;; https://www.emacswiki.org/emacs/Calc#toc14
(defun calc-eval-region (arg beg end)
  "Calculate the region and display the result in the echo area.

With prefix ARG non-nil, insert the result at the end of region.
BEG for beginning, END for end."
  (interactive "P\nr")
  (let* ((expr (buffer-substring-no-properties beg end))
         (result (calc-eval expr)))
    (if (null arg)
        (message "%s = %s" expr result)
      (goto-char end)
      (save-excursion
        (insert result)))))


;; h/t https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
(run-with-idle-timer
 5
 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))


(require 'dired+)


(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Function which colorises the compilation buffer."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; allow remembering risky variables
(defun risky-local-variable-p (sym &optional _ignored) nil)


;;; init.el ends here
