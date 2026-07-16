;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Richard Goulter"
      user-mail-address "richard.goulter@gmail.com")

(setq doom-leader-alt-key "M-SPC")

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

;; Gruvbox light/dark follows OS appearance (Kitty, Ghostty, Zellij, Helix, etc.).
(use-package! auto-dark
  :defer t
  :init
  (setq! auto-dark-themes '((doom-gruvbox) (doom-gruvbox-light))
         doom-theme nil
         custom-safe-themes t)
  (when (eq system-type 'darwin)
    (setq auto-dark-allow-osascript t))
  (defun rgoulter/auto-dark-init-h ()
    (auto-dark-mode)
    (remove-hook 'server-after-make-frame-hook #'rgoulter/auto-dark-init-h)
    (remove-hook 'after-init-hook #'rgoulter/auto-dark-init-h))
  (add-hook (if (daemonp) 'server-after-make-frame-hook 'after-init-hook)
            #'rgoulter/auto-dark-init-h -95))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org")

(defvar workstation-file "File location for a local Emacs Lisp configuration file")
(setq workstation-file (concat "~/.config/doom/" "local.el"))
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

(add-load-path! "lisp")

;; Poll file buffers so agent/bash edits on disk propagate without prompts.
;; Supersedes Doom's lazy `doom-auto-revert-mode` (switch-buffer only).
(use-package! autorevert
  :defer t
  :config
  (setq auto-revert-interval 2
        auto-revert-verbose nil)
  (global-auto-revert-mode +1))

(use-package! agent-shell
  :hook (agent-shell-mode . doom-disable-line-numbers-h)
  :custom
  ;; Graphical header shows the agent icon plus two text rows (~5 lines).
  ;; Text mode keeps agent, model, and project on a single header line.
  ;; Icon fetch needs agent-shell-grok's lobe-icons URL fix (see lisp/).
  (agent-shell-header-style 'text)
  :config
  (require 'agent-shell-grok)
  (agent-shell-grok-setup)
  ;; No preferred agent: M-x agent-shell / SPC o l a prompts among configs.
  ;; Direct starts for the agents we use (others stay available in the picker).
  (map! :leader
        (:prefix ("o l" . "LLM / agent-shell")
         :desc "Agent picker" "a" #'agent-shell
         :desc "Claude Code"  "c" #'agent-shell-anthropic-start-claude-code
         :desc "Codex"        "x" #'agent-shell-openai-start-codex
         :desc "Cursor"       "u" #'agent-shell-cursor-start-agent
         :desc "Grok"         "g" #'grok-start-agent
         :desc "Pi"           "p" #'agent-shell-pi-start-agent))
  ;; evil-collection binds `gs' to cycle-session-mode (shadows Doom easymotion).
  ;; Hook runs after their setup; cycle mode stays on C-<tab>.
  ;; Re-run if setup already happened (after-load order / doom/reload).
  (defun +agent-shell-restore-gs-easymotion-h (mode keymaps &rest _)
    (when (eq mode 'agent-shell)
      (require 'evil-easymotion)
      (dolist (map-sym keymaps)
        (when-let ((map (and (boundp map-sym) (symbol-value map-sym))))
          (evil-define-key* 'normal map
            "gs" (cons "Easymotion" evilem-map))))))
  (add-hook 'evil-collection-setup-hook #'+agent-shell-restore-gs-easymotion-h)
  (when (featurep 'evil-collection-agent-shell)
    (+agent-shell-restore-gs-easymotion-h
     'agent-shell
     (bound-and-true-p evil-collection-agent-shell-maps))))

(use-package! blamer
  :general ("s-i" #'blamer-show-commit-info)
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :height 140
                    :italic t))))

(use-package! discover-my-major
 :config
 (map! :leader :n "h C-m" 'discover-my-major))

(use-package! justl
  :config
  (map! :leader
        :desc "Just choose recipe" "o j c" #'justl-exec-recipe-in-dir
        :desc "Just default recipe" "o j j" #'justl-exec-default-recipe))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

;; https://github.com/felipeochoa/rjsx-mode/issues/85
(add-hook 'rjsx-mode-hook (lambda () (setq-local indent-line-function 'js-jsx-indent-line)))

(use-package! whisper
  :commands (whisper-run whisper-file whisper-select-language rk/select-default-audio-device)
  :config
  ;; Use Nix home.packages: ffmpeg + whisper-cpp (prebuilt whisper-cli).
  ;; Skip whisper.el's runtime git/cmake/g++ install of whisper.cpp.
  (setq whisper-install-whispercpp nil
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        ;; After insert, leave point at end of dictation (natural for drafting).
        whisper-return-cursor 'end)
  ;; macOS: AVFoundation mic index for ffmpeg (`:N`). Pick with SPC o w d.
  ;; https://github.com/natrys/whisper.el/wiki/MacOS-Configuration
  (when (eq system-type 'darwin)
    (require 'ffmpeg-device))
  (defun rgoulter/whisper-model-file ()
    "Path to ggml model under Doom cache (download once with whisper-cpp-download-ggml-model)."
    (expand-file-name
     (format "ggml-%s.bin" whisper-model)
     (expand-file-name "whisper/models/" doom-cache-dir)))
  (defun whisper-command (input-file)
    "Run Nix-provided whisper-cli on INPUT-FILE (overrides package default)."
    (let ((model (rgoulter/whisper-model-file)))
      (unless (file-readable-p model)
        (user-error "Whisper model missing: %s\nDownload with: mkdir -p %s && cd %s && whisper-cpp-download-ggml-model %s"
                    model
                    (file-name-directory model)
                    (file-name-directory model)
                    whisper-model))
      `("whisper-cli"
        ,@(when whisper-use-threads
            (list "--threads" (number-to-string whisper-use-threads)))
        ,@(when whisper-translate '("--translate"))
        ,@(when whisper-show-progress-in-mode-line '("--print-progress"))
        "--language" ,whisper-language
        "--model" ,model
        "--no-timestamps"
        "--file" ,input-file)))
  (map! :leader
        (:prefix ("o w" . "whisper")
         :desc "Whisper record/transcribe" "w" #'whisper-run
         :desc "Whisper from file"         "f" #'whisper-file
         :desc "Whisper language"          "l" #'whisper-select-language
         :when (eq system-type 'darwin)
         :desc "Whisper audio device"      "d" #'rk/select-default-audio-device)))

(use-package! zetteldeft
  :config
  ;; If creating more than one note in a minute
  ;; using zetteldeft, the default id generation
  ;; gives them the same ID.
  (setq zetteldeft-id-format "%Y-%m-%d-%H%M%S")
  (map! :leader
        :prefix ("D" . "deft")
        :desc "deft"                    "d" #'deft
        :desc "new search"              "D" #'zetteldeft-deft-new-search
        :desc "refresh"                 "R" #'deft-refresh
        :desc "search at point"         "s" #'zetteldeft-search-at-point
        :desc "search current id"       "c" #'zetteldeft-search-current-id
        :desc "follow link"             "f" #'zetteldeft-follow-link
        :desc "avy file other window"   "F" #'zetteldeft-avy-file-search-ace-window
        :desc "avy link search"         "l" #'zetteldeft-avy-link-search
        :desc "avy tag search"          "t" #'zetteldeft-avy-tag-search
        :desc "tag list"                "T" #'zetteldeft-tag-buffer
        :desc "insert id"               "i" #'zetteldeft-find-file-id-insert
        :desc "insert full title"       "I" #'zetteldeft-find-file-full-title-insert
        :desc "find file"               "o" #'zetteldeft-find-file
        :desc "new file"                "n" #'zetteldeft-new-file
        :desc "new file & link"         "N" #'zetteldeft-new-file-and-link
        :desc "rename"                  "r" #'zetteldeft-file-rename))


(defun rgoulter/rustic-cargo-build ()
  "run cargo build, switch to that buffer, zoom in on it."
  (interactive)
  (when
    rustic-compilation-directory
    (pop-to-buffer-same-window (get-buffer rustic-compilation-directory)))
  (rustic-cargo-build)
  (pop-to-buffer-same-window (get-buffer rustic-compilation-buffer-name))
  (doom/window-maximize-buffer))

(defun rgoulter/rustic-cargo-test ()
  "run cargo test, switch to that buffer, zoom in on it."
  (interactive)
  (when
    rustic-compilation-directory
    (pop-to-buffer-same-window (get-buffer rustic-compilation-directory)))
  (rustic-cargo-test)
  (pop-to-buffer-same-window (get-buffer rustic-test-buffer-name))
  (doom/window-maximize-buffer))

(defun rgoulter/rustic-cargo-run-nextest ()
  "run cargo test, switch to that buffer, zoom in on it."
  (interactive)
  (when
    rustic-compilation-directory
    (pop-to-buffer-same-window (get-buffer rustic-compilation-directory)))
  (rustic-cargo-run-nextest)
  (pop-to-buffer-same-window (get-buffer rustic-test-buffer-name))
  (doom/window-maximize-buffer))

(defun rgoulter/rustic-cargo-nextest-current-test ()
  "run cargo test, switch to that buffer, zoom in on it."
  (interactive)
  (when
    rustic-compilation-directory
    (pop-to-buffer-same-window (get-buffer rustic-compilation-directory)))
  (rustic-cargo-nextest-current-test)
  (pop-to-buffer-same-window (get-buffer rustic-test-buffer-name))
  (doom/window-maximize-buffer))

(map!
      :localleader
      :map (rustic-mode-map rustic-compilation-mode-map rustic-cargo-test-mode-map)
      "r b"
	  #'rgoulter/rustic-cargo-build)

(map!
      :localleader
      :map (rustic-mode-map rustic-compilation-mode-map rustic-cargo-test-mode-map)
      "r t"
	  #'rgoulter/rustic-cargo-test)

(map!
      :localleader
      :map (rustic-mode-map rustic-compilation-mode-map rustic-cargo-test-mode-map)
      "r T"
	  #'rgoulter/rustic-cargo-nextest-current-test)

(map!
      :localleader
      :map (rustic-mode-map rustic-compilation-mode-map rustic-cargo-test-mode-map)
      "r n"
	  #'rgoulter/rustic-cargo-run-nextest)

;; Kludge: use gpg-agent as the ssh agent
(when (eq system-type 'gnu/linux)
  (setenv "SSH_AUTH_SOCK" (format "/run/user/%d/gnupg/S.gpg-agent.ssh" (user-uid))))
(when (eq system-type 'darwin)
  (setenv "SSH_AUTH_SOCK" (format "%s/.gnupg/S.gpg-agent.ssh" (getenv "HOME"))))

(after! avy
  (setq avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(after! dirvish
  (map! :leader
        :desc "Dirvish preview (DWIM)" "o m" #'dirvish-dwim
        :desc "Dirvish preview"        "o /" #'dirvish
        :desc "Dirvish sidebar"         "o p" #'dirvish-side))

(after! epa
  ;; don't use minibuffer
  (setq epa-pinentry-mode 'ask))

(after! ispell
  ;; British English via aspell (aspellDicts.en in Nix); Oxford style: -ize verbs, -our/-re (colour, organize).
  (setq ispell-dictionary "en_GB-ize"))

(after! magit
  ;; Free [w/]w and M-1..M-9 workspace keys; use gj/gk and z1..z4 for sections.
  (map! :map magit-mode-map
        :nv "[" nil
        :nv "]" nil)
  (undefine-key! magit-mode-map
    "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9")
  (undefine-key! magit-section-mode-map
    "M-1" "M-2" "M-3" "M-4" "M-5" "M-6" "M-7" "M-8" "M-9"))

(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.devenv\\'"))
