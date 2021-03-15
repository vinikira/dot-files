;;; lang-elm.el --- Elm Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elm mode
;; =============================================================================
(straight-use-package 'elm-mode)

(add-to-list 'auto-mode-alist '("\\.elm$" . elm-mode))
;; =============================================================================

(provide 'lang-elm)

;;; lang-elm.el ends here
