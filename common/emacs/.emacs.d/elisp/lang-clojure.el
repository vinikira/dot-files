;;; lang-clojure.el --- Clojure Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Clojure mode
;; =============================================================================
(straight-use-package 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; =============================================================================

;; Cider
;; =============================================================================
(straight-use-package 'cider)
;; =============================================================================

(provide 'lang-clojure)
;;; lang-clojure.el ends here
