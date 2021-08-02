;;; layer-lsp.el --- LSP Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Toggle LSP per project
;; =============================================================================
(defvar vs/lsp-allowed-projects '()
  "Projects that are allowed to use LSP in current session.")

(declare-function projectile-project-root "ext:projectile")
(declare-function projectile-project-buffers "ext:projectile")
(declare-function projectile-buffers-with-file "ext:projectile")
(declare-function lsp-deferred "ext:lsp-mode")
(declare-function lsp-disconnect "ext:lsp-mode")

(defun vs/toggle-lsp-for-current-project ()
  "Enable/disable `lsp-mode' for current project."
  (interactive)
  (let ((disabled nil)
	(current-project (projectile-project-root)))
    (when (eq current-project nil)
      (error "The current buffer is not part of any project"))
    (if (member current-project vs/lsp-allowed-projects)
	(progn
	  (setq disabled t
		vs/lsp-allowed-projects
		(delete current-project vs/lsp-allowed-projects)))
      (push current-project vs/lsp-allowed-projects))
    (dolist (file (projectile-buffers-with-file (projectile-project-buffers)))
      (with-current-buffer file
	(if disabled
	    (lsp-disconnect)
          (lsp-deferred))))))
;; =============================================================================


;; LSP Mode
;; =============================================================================
(straight-use-package 'lsp-mode)

(defconst vs/lsp-ignore-files
  '("\\.asdf" "[/\\\\]\\.elixir_ls$"
    "[/\\\\]deps$" "[/\\\\]_build$"))

(with-eval-after-load 'lsp-mode
  (declare-function lsp-execute-code-action "ext:lsp-mode")
  (declare-function lsp-format-buffer "ext:lsp-mode")
  (declare-function lsp-describe-thing-at-point "ext:lsp-mode")
  (declare-function lsp-rename "ext:lsp-mode")
  (declare-function lsp-find-references "ext:lsp-mode")

  (when (boundp 'lsp-file-watch-ignored-directories)
    (customize-set-variable 'lsp-file-watch-ignored-directories
                            (append vs/lsp-ignore-files lsp-file-watch-ignored-directories)))
  (when (boundp 'lsp-mode-map)
    (define-key lsp-mode-map (kbd "M-RET") #'lsp-execute-code-action)
    (define-key lsp-mode-map (kbd "C-c C-f") #'lsp-format-buffer)
    (define-key lsp-mode-map (kbd "C-c C-d") #'lsp-describe-thing-at-point)
    (define-key lsp-mode-map (kbd "C-c C-r") #'lsp-rename)
    (define-key lsp-mode-map (kbd "C-c C-g") #'lsp-find-references)))

(customize-set-variable 'lsp-auto-guess-root t)
(customize-set-variable 'lsp-keymap-prefix "H-l")
(customize-set-variable 'lsp-modeline-diagnostics-scope :project)
(customize-set-variable 'lsp-keep-workspace-alive nil)
(customize-set-variable 'lsp-signature-render-documentation nil)
(customize-set-variable 'lsp-headerline-breadcrumb-enable nil)
(customize-set-variable 'lsp-enable-links nil)
(customize-set-variable 'lsp-log-io nil)
(customize-set-variable 'lsp-completion-provider :none)

(declare-function lsp-deferred "ext:lsp-mode")
(declare-function lsp-enable-which-key-integration "ext:lsp-mode")

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'prog-mode-hook
	  (lambda ()
	    (when (member (projectile-project-root) vs/lsp-allowed-projects)
	      (lsp-deferred))))
;; =============================================================================

;; LSP UI
;; =============================================================================
(straight-use-package 'lsp-ui)

(customize-set-variable 'lsp-ui-doc-include-signature t)
(customize-set-variable 'lsp-ui-doc-enable nil)
(customize-set-variable 'lsp-ui-sideline-enable nil)
;; =============================================================================

;; DAP Mode
;; =============================================================================
(straight-use-package 'dap-mode)

(declare-function dap-debug "ext:dap-mode")

(global-set-key (kbd "<f12>") #'dap-debug)

(with-eval-after-load 'dap-mode
  (declare-function dap-hydra "ext:dap-hydra")
  (declare-function dap-breakpoint-toggle "ext:dap-mode")

  (when (boundp 'dap-mode-map)
    (define-key dap-mode-map (kbd "S-<f12>") #'dap-hydra)
    (define-key dap-mode-map (kbd "<f9>") #'dap-breakpoint-toggle))

  (add-hook 'python-mode-hook (lambda () (require 'dap-python)))
  (add-hook 'java-mode-hook (lambda () (require 'dap-java)))
  (add-hook 'c-mode-hook (lambda () (require 'dap-lldb)))
  (add-hook 'c++-mode-hook (lambda () (require 'dap-lldb)))
  (add-hook 'php-mode-hook (lambda () (require 'dap-php)))
  (add-hook 'elixir-mode-hook (lambda () (require 'dap-elixir)))
  (add-hook 'js-mode-hook (lambda () (require 'dap-chrome)))
  (add-hook 'typescript-mode-hook (lambda () (require 'dap-chrome)))
  (add-hook 'rjsx-mode-hook (lambda () (require 'dap-chrome)))
  (add-hook 'rust-mode-hook (lambda () (require 'dap-gdb-lldb))))
;; =============================================================================

;; Consult LSP
;; =============================================================================
(straight-use-package
 '(consult-lsp :type git :host github :branch "main" :repo "gagbo/consult-lsp"))
;; =============================================================================

(provide 'layer-lsp)

;;; layer-lsp.el ends here
