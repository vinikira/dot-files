;;; layer-lsp.el --- LSP Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; LSP Mode
;; =============================================================================
(straight-use-package 'lsp-mode)

(defconst vs/lsp-ignore-files
  '("\\.asdf" "[/\\\\]\\.elixir_ls$"
    "[/\\\\]deps$" "[/\\\\]_build$"))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "M-RET") #'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c C-f") #'lsp-format-buffer)
  (customize-set-variable 'lsp-file-watch-ignored
                          (append vs/lsp-ignore-files lsp-file-watch-ignored)))

(customize-set-variable 'lsp-auto-guess-root t)
(customize-set-variable 'lsp-keymap-prefix "H-l")
(customize-set-variable 'lsp-rust-server 'rust-analyzer)
(customize-set-variable 'lsp-idle-delay 0.500)
(customize-set-variable 'lsp-modeline-diagnostics-scope :project)
(customize-set-variable 'lsp-keep-workspace-alive nil)
(customize-set-variable 'lsp-completion-enable-additional-text-edit nil)
(customize-set-variable 'lsp-lens-enable t)

(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'elixir-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'java-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'dart-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'kotlin-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)
;; =============================================================================

;; LSP Ivy
;; ============================================================================
(straight-use-package 'lsp-ivy)

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "H-l f") #'lsp-ivy-workspace-symbol))
;; =============================================================================

;; LSP UI
;; =============================================================================
(straight-use-package 'lsp-ui)

(customize-set-variable 'lsp-ui-doc-include-signature t)
(customize-set-variable 'lsp-ui-doc-header t)
(customize-set-variable 'lsp-ui-doc-enable nil)
(customize-set-variable 'lsp-ui-sideline-enable nil)
;; =============================================================================

;; DAP Mode
;; =============================================================================
(straight-use-package 'dap-mode)

(global-set-key (kbd "<f12>") #'dap-debug)

(with-eval-after-load 'dap-mode
  (define-key dap-mode-map (kbd "S-<f12>") #'dap-debug-hydra)
  (define-key dap-mode-map (kbd "<f9>") #'dap-breakpoint-toggle)
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

(provide 'layer-lsp)

;;; layer-lsp.el ends here
