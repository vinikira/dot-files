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

;; Flycheck CLJ-Kondo
;; =============================================================================
(straight-use-package 'flycheck-clj-kondo)

(add-hook 'clojure-mode-hook
          (lambda () (require 'flycheck-clj-kondo)))
;; =============================================================================

(provide 'lang-clojure)
;;; lang-clojure.el ends here
