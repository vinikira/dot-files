;;; lang-clojure.el --- Clojure Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Clojure mode
;; =============================================================================
(straight-use-package 'clojure-mode)
;; =============================================================================

;; Cider
;; =============================================================================
(straight-use-package 'cider)
;; =============================================================================

(provide 'lang-clojure)
;;; lang-clojure.el ends here
