(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-linum-mode t)
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
  (push '("marmalade" . "http://marmalade-repo.org/packages/")
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

(req-package solarized-theme
  :config (progn
            (load-theme 'solarized-dark t)))

;; 'Borrowed' by searching GitHub for "use-package tuareg"
(req-package tuareg
  :ensure t
  ; :load-path ("~/.opam/system/share/emacs/site-lisp/")

  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)
         ("\\.topscript$" . tuareg-mode))

  :config (use-package merlin
            :init (setq merlin-use-auto-complete-mode t
                        merlin-error-after-save nil)

            :config (add-hook 'tuareg-mode-hook 'merlin-mode)

            :bind (("C-c <up>" . merlin-type-enclosing-go-up)
                   ("C-c <down>" . merlin-type-enclosing-go-down))))

(req-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

(req-package-finish)
