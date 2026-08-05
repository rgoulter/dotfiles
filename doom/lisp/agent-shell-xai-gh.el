;;; agent-shell-xai-gh.el --- GH_TOKEN for sandboxed Grok Build sessions -*- lexical-binding: t; -*-
;;
;; Grok Build runs tool shells under an OS sandbox that cannot use the macOS
;; Keychain.  `gh` then needs either `oauth_token` in ~/.config/gh/hosts.yml or
;; GH_TOKEN / GITHUB_TOKEN in the ACP process environment.
;;
;; This module injects those env vars when starting the Grok client, reading
;; the token from password-store (default entry token/gh).  Override or disable
;; via `local.el' — see local.el.template.

;;; Code:

(require 'map)
(require 'subr-x)

(defvar +agent-shell-xai-gh-pass-entry "token/gh"
  "Pass entry for a GitHub token injected as GH_TOKEN/GITHUB_TOKEN.

Used when Keychain auth is unavailable in sandboxed Grok/bash tools.
Set to nil to disable pass lookup.  Overridden by `+agent-shell-xai-gh-token'.")

(defvar +agent-shell-xai-gh-token nil
  "When non-nil, GH/GITHUB_TOKEN for the Grok ACP client (skips pass lookup).
Set in `local.el' to override `+agent-shell-xai-gh-pass-entry'.")

(defun +agent-shell-xai-gh--token ()
  "Return a GitHub token for the Grok ACP client, or nil."
  (or +agent-shell-xai-gh-token
      (and +agent-shell-xai-gh-pass-entry
           (require 'password-store nil t)
           (fboundp 'password-store-get)
           (let ((token (ignore-errors
                          (password-store-get +agent-shell-xai-gh-pass-entry))))
             (and token
                  (not (string-empty-p (string-trim token)))
                  (string-trim token))))))

(defun +agent-shell-xai-gh--env-vars ()
  "Return (\"GH_TOKEN=…\" \"GITHUB_TOKEN=…\") when a token is available."
  (when-let ((token (+agent-shell-xai-gh--token)))
    (list (format "GH_TOKEN=%s" token)
          (format "GITHUB_TOKEN=%s" token))))

(defun +agent-shell-xai-gh--make-client-around (orig &rest args)
  "Around advice: prepend GH tokens to the ACP client's environment-variables."
  (let ((client (apply orig args)))
    (when-let ((gh-env (+agent-shell-xai-gh--env-vars)))
      (setf (map-elt client :environment-variables)
            (append gh-env (map-elt client :environment-variables))))
    client))

;;;###autoload
(defun +agent-shell-xai-gh-setup ()
  "Inject GH_TOKEN into Grok Build ACP sessions from pass / override vars.

Advises `agent-shell-xai-make-client' so the token is resolved when the
agent starts (not at Emacs init).  Safe to call more than once."
  (advice-add 'agent-shell-xai-make-client :around
              #'+agent-shell-xai-gh--make-client-around))

(provide 'agent-shell-xai-gh)
;;; agent-shell-xai-gh.el ends here
