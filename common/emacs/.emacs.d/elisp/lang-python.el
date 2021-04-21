;;; lang-python.el --- Python Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Python Mode
;; =============================================================================
(customize-set-variable 'python-shell-interpreter "ipython")
(customize-set-variable 'python-shell-interpreter-args "-i --simple-prompt")
;; =============================================================================

;; LSP Python
;; =============================================================================
(straight-use-package 'lsp-python-ms)

(customize-set-variable 'lsp-python-ms-auto-install-server t)
;; =============================================================================

(provide 'lang-python)

;;; lang-python.el ends here
