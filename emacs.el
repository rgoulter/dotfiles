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

;; Use ibuffer mode, which manages buffers like Dired manages directories.
;; From http://tuhdo.github.io/emacs-tutor.html
(global-set-key (kbd "C-x C-b") 'ibuffer)


(require 'package)
  (push '("marmalade" . "https://marmalade-repo.org/packages/")
        package-archives )
  (push '("melpa" . "http://melpa.milkbox.net/packages/")
        package-archives)
  (push '("melpa-stable" . "http://stable.melpa.org/packages/")
        package-archives)

; Taken from http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
; See also: http://www.emacswiki.org/emacs/ELPA
(setq package-enable-at-startup nil)
(package-initialize)

(require 'req-package)

(req-package evil
  :config (evil-mode 1))

;; https://github.com/sellout/emacs-color-theme-solarized
; 930-stars on GitHub
;; (req-package color-theme-solarized
;;   :require color-theme
;;   :config (progn
;;             ; (set-frame-parameter nil 'background-mode 'dark) ; for GUI-only. I guess this breaks in term.
;;             (load-theme 'solarized t)))

;; 'Borrowed' by searching GitHub for "use-package tuareg"
(req-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)
         ("\\.topscript$" . tuareg-mode)))

(req-package merlin
  :init (setq ; merlin-use-auto-complete-mode t
              merlin-error-after-save nil)

  :config (add-hook 'tuareg-mode-hook 'merlin-mode)
          ; See http://emacs.stackexchange.com/questions/12084/how-to-get-merlin-mode-to-work-in-emacs
          ;(setq merlin-command 'opam)

  ;; For some reason, having :bind here throws this out of whack. ???
  ; :bind (("C-c <up>" . merlin-type-enclosing-go-up)
  ;        ("C-c <down>" . merlin-type-enclosing-go-down))
  )

(req-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

; https://github.com/istib/rainbow-blocks looks maybe pretty, also?
; use M-x rainbow-delimiters-mode to toggle. (EVIL: Can use as Ex command).
(req-package rainbow-delimiters
  ; Enable for most programming languages:
  :config (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(req-package org)
(req-package evil-leader)
(req-package evil-org)

; Qn: Why is it (add-hook ... #'whatever-mode) vs (add-hook ... 'whatever-mode) ???

(req-package-finish)



; Reload Emacs settings
; taken from http://www.saltycrane.com/blog/2007/07/how-to-reload-your-emacs-file-while/
(defun reload-dotemacs-file ()
    "reload your .emacs file without restarting Emacs"
    (interactive)
    (load-file "~/.emacs"))



; Set to dark or light
;? These won't work if use `M-x customize-variable frame-background-mode`.
;? What's an idiomatic way of enabling something like this?
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




(setq org-agenda-files '("~/org-files/programming/tools.org"
                         "~/org-files/personal/finance.org"
                         "~/org-files/personal/vietnam.org"
                         "~/org-files/books.org"
                         "~/org-files/games.org"))

; from: https://orgmode.org/manual/Activation.html#Activation
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

; from: http://sachachua.com/blog/2015/02/learn-take-notes-efficiently-org-mode/
; use C-x r j (jump-to-register)
(set-register ?o (cons 'file "~/org-files/capture.org"))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))

(setq org-default-notes-file "~/org-files/capture.org")

; ido, is easier to get started with than helm
(ido-mode)
(setq org-completion-use-ido t)
