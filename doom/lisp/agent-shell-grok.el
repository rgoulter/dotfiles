;;; agent-shell-grok.el --- Grok Build agent for agent-shell -*- lexical-binding: t; -*-

(require 'agent-shell)
(require 'ansi-color)

(defvar agent-shell-grok-acp-command '("grok" "agent" "stdio")
  "Command and parameters for the Grok ACP client.")

(defvar agent-shell-grok--stripped-env-prefixes
  '("AWS_ACCESS_KEY_ID="
    "AWS_DEFAULT_PROFILE="
    "AWS_PROFILE="
    "AWS_SECRET_ACCESS_KEY="
    "AWS_SESSION_TOKEN="
    "GH_TOKEN="
    "GITHUB_TOKEN="
    "GIT_ASKPASS="
    "GPG_AGENT_INFO="
    "PASS_PASSWORD_STORE_DIR="
    "SSH_AGENT_PID="
    "SSH_AUTH_SOCK=")
  "Env var prefixes dropped from the Grok ACP client environment.

Agent sessions should not inherit human SSH/GPG agents or cloud tokens.
Stripped GH/GITHUB_TOKEN are replaced from pass when configured — see
`agent-shell-grok-gh-pass-entry'.  Optional deploy-key ssh-agent:
`agent-shell-grok-agent-ssh-socket'.")

(defvar agent-shell-grok-agent-ssh-socket nil
  "When non-nil, `SSH_AUTH_SOCK' for the Grok ACP client (e.g. deploy-key agent).
Set in `local.el' when using a dedicated agent ssh-agent.")

(defvar agent-shell-grok-gh-pass-entry "token/gh"
  "Pass entry for a GitHub token injected as GH_TOKEN/GITHUB_TOKEN.

Used when Keychain auth is unavailable in sandboxed Grok/bash tools.  Set to
nil to disable pass lookup.  Overridden by `agent-shell-grok-gh-token'.")

(defvar agent-shell-grok-gh-token nil
  "When non-nil, GH/GITHUB_TOKEN for the Grok ACP client (skips pass lookup).
Set in `local.el' to override `agent-shell-grok-gh-pass-entry'.")

(defun agent-shell-grok--drop-env-entry-p (entry)
  "Return non-nil if ENTRY should not be passed to the Grok ACP client."
  (or (seq-some (lambda (pfx) (string-prefix-p pfx entry))
                agent-shell-grok--stripped-env-prefixes)
      (string-prefix-p "CARGO_TERM_COLOR=" entry)
      (string-prefix-p "CARGO_TERM_PROGRESS_WHEN=" entry)
      (string-prefix-p "CLICOLOR_FORCE=" entry)
      (string-prefix-p "CLICOLOR=" entry)
      (string-prefix-p "COLORTERM=" entry)
      (string-prefix-p "FORCE_COLOR=" entry)
      (string-prefix-p "NO_COLOR=" entry)))

(defun agent-shell-grok--gh-token ()
  "Return a GitHub token for the Grok ACP client, or nil."
  (or agent-shell-grok-gh-token
      (and agent-shell-grok-gh-pass-entry
           (require 'password-store nil t)
           (fboundp 'password-store-get)
           (let ((token (password-store-get agent-shell-grok-gh-pass-entry)))
             (and token (not (string-empty-p (string-trim token)))
                  (string-trim token))))))

(defun agent-shell-grok--environment ()
  "Environment for the Grok ACP client.

Inherit Emacs's environment, but force monochrome output and drop auth
sockets and credential env vars so Grok/bash tools do not run as you.
Inject agent-scoped GH/GITHUB_TOKEN from pass when configured."
  (let ((env (append '("CARGO_TERM_COLOR=never"
                        "CARGO_TERM_PROGRESS_WHEN=never"
                        "CLICOLOR=0"
                        "FORCE_COLOR=0"
                        "NO_COLOR=1")
                      (seq-filter
                       (lambda (entry)
                         (not (agent-shell-grok--drop-env-entry-p entry)))
                       (agent-shell-make-environment-variables :inherit-env t)))))
    (when agent-shell-grok-agent-ssh-socket
      (setq env (append (list (format "SSH_AUTH_SOCK=%s"
                                      agent-shell-grok-agent-ssh-socket))
                        (seq-filter
                         (lambda (entry)
                           (not (string-prefix-p "SSH_AUTH_SOCK=" entry)))
                         env))))
    (when-let ((token (agent-shell-grok--gh-token)))
      (setq env (append (list (format "GH_TOKEN=%s" token)
                              (format "GITHUB_TOKEN=%s" token))
                        env)))
    env))

(defun agent-shell-grok--sanitize-tool-output (text)
  "Return TEXT with ANSI color sequences removed."
  (if (stringp text)
      (ansi-color-filter-apply text)
    text))

(defun agent-shell-grok--sanitize-fragment--advice (orig &rest args)
  "Strip ANSI escapes from Grok tool output before agent-shell renders it."
  (let ((kw args))
    (when-let* ((state (plist-get kw :state))
                ((eq (map-nested-elt state '(:agent-config :identifier)) 'grok))
                (body (plist-get kw :body)))
      (setq kw (plist-put kw :body (agent-shell-grok--sanitize-tool-output body))))
    (apply orig kw)))

(defun agent-shell-grok--ensure-output-sanitize-advice ()
  "Install one-time advice stripping ANSI from Grok tool-call fragments."
  (unless (get 'agent-shell--update-fragment 'agent-shell-grok-output-sanitize)
    (put 'agent-shell--update-fragment 'agent-shell-grok-output-sanitize t)
    (advice-add 'agent-shell--update-fragment :around
                #'agent-shell-grok--sanitize-fragment--advice)))

(defun agent-shell-grok--auto-revert-stale-buffer--advice (orig &rest args)
  "Reload unmodified buffers before agent-shell reads them from disk.

When the agent edits via bash tools, open buffers can be stale.  agent-shell
prompts to reload on `fs/read_text_file'; skip that when the user has no
unsaved edits."
  (let ((kw (car args)))
    (when-let ((buffer (plist-get kw :buffer)))
      (with-current-buffer buffer
        (when (and (buffer-file-name)
                   (not (verify-visited-file-modtime))
                   (not (buffer-modified-p)))
          (revert-buffer t nil nil))))
    (apply orig args)))

(defun agent-shell-grok--ensure-auto-revert-buffer-advice ()
  "Install one-time advice auto-reloading stale unmodified file buffers."
  (unless (get 'agent-shell--extract-buffer-text 'agent-shell-grok-auto-revert)
    (put 'agent-shell--extract-buffer-text 'agent-shell-grok-auto-revert t)
    (advice-add 'agent-shell--extract-buffer-text :around
                #'agent-shell-grok--auto-revert-stale-buffer--advice)))

(defun agent-shell-grok-make-agent-config ()
  "Create a Grok agent configuration."
  (agent-shell-make-agent-config
   :identifier 'grok
   :mode-line-name "Grok"
   :icon-name "grok.png"
   :buffer-name "Grok"
   :shell-prompt "Grok> "
   :shell-prompt-regexp "Grok> "
   :client-maker
   (lambda (buffer)
     (agent-shell--make-acp-client
      :command (car agent-shell-grok-acp-command)
      :command-params (cdr agent-shell-grok-acp-command)
      :environment-variables (agent-shell-grok--environment)
      :context-buffer buffer))
   :install-instructions "Install Grok Build and run `grok login` in a terminal."))

;;;###autoload
(defun grok-start-agent ()
  "Start an interactive Grok ACP shell."
  (interactive)
  (agent-shell--dwim :config (agent-shell-grok-make-agent-config) :new-shell t))

(defun agent-shell-grok--fix-icon-fetch-advice (orig-fn icon-name)
  "Rewrite lobe-icons filenames to working raw.githubusercontent.com URLs.

agent-shell builds URLs with refs/heads/master, which GitHub rejects
with HTTP 400. Full https URLs are passed through unchanged."
  (when (and icon-name
             (not (string-prefix-p "https://" (downcase icon-name)))
             (not (string-prefix-p "http://" (downcase icon-name))))
    (setq icon-name
          (concat "https://raw.githubusercontent.com/lobehub/lobe-icons/master/packages/static-png/"
                  (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "light")
                  "/" icon-name)))
  (funcall orig-fn icon-name))

(defun agent-shell-grok--ensure-icon-fetch-fix ()
  "Install one-time advice fixing agent-shell lobe-icons URL template."
  (unless (get 'agent-shell--fetch-agent-icon 'agent-shell-grok-icon-fix)
    (put 'agent-shell--fetch-agent-icon 'agent-shell-grok-icon-fix t)
    (advice-add 'agent-shell--fetch-agent-icon :around #'agent-shell-grok--fix-icon-fetch-advice)))

(defun agent-shell-grok-setup ()
  "Register Grok with agent-shell.

Does not set `agent-shell-preferred-agent-config'; leave that nil so
`agent-shell' prompts, or set it elsewhere if you want a default."
  (agent-shell-grok--ensure-icon-fetch-fix)
  (agent-shell-grok--ensure-output-sanitize-advice)
  (agent-shell-grok--ensure-auto-revert-buffer-advice)
  (add-to-list 'agent-shell-agent-configs (agent-shell-grok-make-agent-config)))

(provide 'agent-shell-grok)
