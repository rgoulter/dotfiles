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

(defun agent-shell-grok-setup ()
  "Register Grok with agent-shell."
  (add-to-list 'agent-shell-agent-configs (agent-shell-grok-make-agent-config))
  (setq agent-shell-preferred-agent-config 'grok))

(defun agent-shell-grok-setup-keys ()
  "Bind Grok agent-shell keys (after `map!' is available)."
  (map! :leader
        :desc "Grok agent" "a g" #'grok-start-agent
        :desc "Agent shell" "a a" #'agent-shell))

(provide 'agent-shell-grok)