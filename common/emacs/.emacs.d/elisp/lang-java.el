;;; lang-java.el --- Java Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Java Mode
;; =============================================================================
(defun vs/--java-hook ()
  "Configures 'java-mode'."
  (require 'cc-mode)
  (c-set-style "cc-mode")
  (make-local-variable 'tab-width)
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'c-basic-offset)
  (customize-set-variable 'tab-width 4)
  (customize-set-variable 'indent-tabs-mode t)
  (customize-set-variable 'c-basic-offset 4))

(add-hook 'java-mode-hook #'vs/--java-hook)
;; =============================================================================

(provide 'lang-java)

;;; lang-java.el ends here
