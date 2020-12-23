;;; lang-groovy.el --- Groovy Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Groovy mode
;; =============================================================================
(straight-use-package 'groovy-mode)

(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))
;; =============================================================================

(provide 'lang-groovy)

;;; lang-groovy.el ends here
