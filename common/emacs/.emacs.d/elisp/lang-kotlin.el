;;; lang-kotlin.el --- Kotlin Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Kotlin Mode
;; =============================================================================
(straight-use-package 'kotlin-mode)

(add-to-list 'auto-mode-alist '("\\.kt$" . kotlin-mode))
;; =============================================================================

(provide 'lang-kotlin)

;;; lang-kotlin.el ends here
