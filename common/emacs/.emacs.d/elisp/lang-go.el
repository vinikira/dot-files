;;; lang-go.el --- Golang Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Go mode
;; =============================================================================
(straight-use-package 'go-mode)

(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
;; =============================================================================

(provide 'lang-go)

;;; lang-go.el ends here
