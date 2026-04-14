;;; cl-lsp-integration.el --- Common Lisp LSP integration with SLY-first arbitration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 OpenCode
;; Author: OpenCode AI System
;; Keywords: lisp, lsp, sly, development
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (lsp-mode "8.0") (lsp-ui "8.0"))

;;; Commentary:
;; Integrates alive-lsp language server with Emacs lsp-mode while preserving
;; SLY as the primary workflow for completion, REPL, and live-image operations.
;;
;; Governing principle (D-07/D-08): SLY-first, LSP-fallback.
;;   - Completion: SLY owns it (lsp-completion-provider :none)
;;   - REPL/eval/debug: SLY only
;;   - Cross-file navigation: LSP (M-. via lsp-find-definition for unloaded files)
;;   - Hover docs: LSP on demand (C-c l d) — NOT auto-popup
;;   - Inline diagnostics: lsp-mode flycheck backend
;;
;; Installation (done by Plan 03):
;;   (require 'cl-lsp-integration)
;;   (cl-lsp-integration-mode 1)

;;; Code:

(require 'lsp-mode nil t)
(require 'lsp-ui nil t)
;; lsp-lisp provides the alive-lsp TCP client; loaded after lsp-mode initializes
;; (lsp-mode adds its clients/ directory to load-path during initialization)
(with-eval-after-load 'lsp-mode
  (require 'lsp-lisp nil t))

(defgroup cl-lsp-integration nil
  "Common Lisp LSP integration with SLY-first arbitration."
  :group 'lsp-mode
  :prefix "cl-lsp-")

(defcustom cl-lsp-port 8006
  "TCP port alive-lsp server listens on."
  :type 'integer
  :group 'cl-lsp-integration)

(defvar cl-lsp-integration-mode nil
  "Non-nil when cl-lsp-integration-mode is active.")

;;; --- Workspace isolation (LSP-03, Claude's Discretion) ---

(with-eval-after-load 'lsp-mode
  ;; Register .asd as a workspace root marker so each ASDF project is isolated.
  ;; lsp-mode uses these to find the rootUri sent in LSP initialize request.
  (add-to-list 'lsp-workspace-root-markers ".asd")
  ;; Large lisp trees under ~/SourceCode/lisp — raise the file watch threshold.
  (setq lsp-file-watch-threshold 3000))

;;; --- SLY-first arbitration (D-07, D-08) ---

(defun cl-lsp-enable ()
  "Enable LSP for the current lisp-mode buffer with SLY-first arbitration.
Sets lsp-completion-provider to :none so SLY CAPF remains primary.
Enables lsp-mode diagnostics and cross-file navigation; disables auto hover.
Called from lisp-mode-hook."
  ;; Disable LSP completion — SLY owns completion in lisp-mode (D-07)
  (setq-local lsp-completion-provider :none)
  ;; Disable automatic lsp-ui-doc popup — SLY has its own doc commands (D-07)
  (setq-local lsp-ui-doc-enable nil)
  ;; Activate lsp-mode (connects to alive-lsp TCP server on port 8006).
  ;; Guard with fboundp so the module loads cleanly in contexts where lsp-mode
  ;; is not yet fully initialised (e.g., batch ERT runs, early-load testing).
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

;;; --- D-09: Enabled LSP features ---
;; 1. Cross-file jump-to-definition: provided by lsp-mode's xref integration (M-.)
;; 2. Hover docs on demand: C-c l d bound below
;; 3. Inline diagnostics: lsp-mode flycheck backend (enabled by default)
;; NOT enabled: workspace-wide symbol search (deferred per D-09)

;;; --- D-05, D-06: File-save sync + manual refresh ---

(defun cl-lsp-restart ()
  "Manually restart the alive-lsp workspace to force full reindex.
Use after significant REPL image changes (new packages, redefined systems)
that lsp-mode's automatic textDocument/didSave does not cover."
  (interactive)
  (if (lsp-workspaces)
      (progn
        (lsp-restart-workspace)
        (message "cl-lsp: workspace restarted"))
    (message "cl-lsp: no active LSP workspace in this buffer")))

;;; --- Keybindings (C-c l prefix, no conflict with SLY C-c prefix) ---

(defvar cl-lsp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Hover doc on demand (D-09, LSP-04) — bound explicitly, not auto-popup
    (define-key map (kbd "C-c l d") #'lsp-ui-doc-show)
    ;; Manual workspace refresh (D-05)
    (define-key map (kbd "C-c l r") #'cl-lsp-restart)
    ;; LSP diagnostics list
    (define-key map (kbd "C-c l e") #'lsp-treemacs-errors-list)
    map)
  "Keymap for cl-lsp-integration-mode.")

;;; --- Minor mode ---

(define-minor-mode cl-lsp-integration-mode
  "Minor mode enabling alive-lsp integration with SLY-first arbitration.
When active, lisp-mode buffers get LSP navigation, hover docs on demand,
and inline diagnostics while SLY retains completion and REPL primacy."
  :lighter " CL-LSP"
  :keymap cl-lsp-mode-map
  :group 'cl-lsp-integration
  (if cl-lsp-integration-mode
      (add-hook 'lisp-mode-hook #'cl-lsp-enable nil t)
    (remove-hook 'lisp-mode-hook #'cl-lsp-enable t)))

;;; --- Global enable on load ---

(add-hook 'lisp-mode-hook #'cl-lsp-enable)

(provide 'cl-lsp-integration)
;;; cl-lsp-integration.el ends here
