;;; lang-crystal.el --- Crystal lang config -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

;; Crystal mode
;; =============================================================================
(straight-use-package 'crystal-mode)
(straight-use-package 'flycheck-crystal)

(with-eval-after-load 'crystal-mode
  (require 'flycheck-crystal))
;; =============================================================================

(provide 'lang-crystal)

;;; lang-crystal.el ends here
