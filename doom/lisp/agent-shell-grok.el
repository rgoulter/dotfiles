;;; agent-shell-grok.el --- Grok Build agent for agent-shell -*- lexical-binding: t; -*-

(require 'agent-shell)

(defvar agent-shell-grok-acp-command '("grok" "agent" "stdio")
  "Command and parameters for the Grok ACP client.")

(defvar agent-shell-grok-environment
  (agent-shell-make-environment-variables :inherit-env t)
  "Environment variables for the Grok client.")

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
      :environment-variables agent-shell-grok-environment
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
  "Register Grok with agent-shell."
  (agent-shell-grok--ensure-icon-fetch-fix)
  (add-to-list 'agent-shell-agent-configs (agent-shell-grok-make-agent-config))
  (setq agent-shell-preferred-agent-config 'grok))

(defun agent-shell-grok-setup-keys ()
  "Bind Grok agent-shell keys (after `map!' is available)."
  (map! :leader
        :desc "Grok agent" "o l g" #'grok-start-agent))

(provide 'agent-shell-grok)
