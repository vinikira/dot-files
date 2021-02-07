;;; lang-haskell.el --- Haskell Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Haskell Mode
;; =============================================================================
(straight-use-package 'haskell-mode)

(customize-set-variable 'haskell-font-lock-symbols t)

(add-hook 'haskell-mode-hook #'haskell-indentation-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
;; =============================================================================

;; LSP Haskell
;; =============================================================================
(straight-use-package 'lsp-haskell)
;; =============================================================================

(provide 'lang-haskell)

;;; lang-haskell.el ends here
