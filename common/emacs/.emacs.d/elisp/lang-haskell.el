;;; lang-haskell.el --- Haskell Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Haskell Mode
;; =============================================================================
(straight-use-package 'haskell-mode)

(customize-set-variable 'haskell-font-lock-symbols t)

(declare-function haskell-indentation-mode "ext:haskell-indentation")
(declare-function interactive-haskell-mode "ext:haskell")

(add-hook 'haskell-mode-hook #'haskell-indentation-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
;; =============================================================================

(provide 'lang-haskell)

;;; lang-haskell.el ends here
